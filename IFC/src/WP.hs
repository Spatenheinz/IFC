{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module WP where

import qualified Data.Map as M
import Control.Monad.State.Strict
import AST
import Control.Monad.Reader
import Data.Maybe
import Data.List (isInfixOf)
import Utils
import Data.Functor

type Count = Int
type Counter = M.Map VName Count
type Env = M.Map VName VName

type WP a = StateT Counter (ReaderT Env (Either String)) a

runWLP :: Foldable t =>Stmt -> (t VName, Maybe FOL) -> Either String (FOL, Counter)
runWLP s (vs, pre) =
  runReaderT (runStateT addPre M.empty) M.empty
  where
    addPre = do
      wlp' <- wlp s $ Cond (BoolConst True)
      pre' <- case pre of
                Nothing -> return wlp'
                Just a -> return $ a .=>. wlp'
      return $ foldr Forall pre' vs

wlp :: Stmt -> FOL -> WP FOL
wlp (Seq s1 s2) q = wlp s2 q >>= wlp s1
wlp Skip q = return q
wlp (GhostAss x a) q = do
    genGhost x
    q' <- resolveQ (Forall x (Cond (RBinary Eq (Ghost x) a) .=>. q))
    fixAExpr a <&> reqs q'
wlp (Assign x a) q = do
    x' <- genVar x
    q' <- local (M.insert x x') $ resolveQ q
    (reqs (Cond (RBinary Eq (Var x') a) .=>. q') <$> fixAExpr a) <&> Forall x'
wlp (If b s1 s2) q = do
  s1' <- wlp s1 q
  s2' <- wlp s2 q
  let b' = Cond b
  fixFOL b' <&> reqs ((b' .=>. s1') ./\. (anegate b' .=>. s2'))
wlp s@(Asst a) q = findVars s [] >> fixFOL a <&> reqs (a ./\. q)
wlp Fail _q = return ffalse
wlp (While b inv var s) q = do
  -- Give back condition for unbounded integer variant
  (fa, var', veq) <- maybe (return (id, ftrue, ftrue)) resolveVar var
  -- wlp(s, I /\ invariant condition)
  inv' <- fixFOL inv
  bs' <- fixBExpr b
  var'' <- maybe (return []) fixAExpr var
  w <- wlp s (inv ./\. var')
  -- check which variables we wanna forall over
  fas <- findVars s []
  let inner = fa $ reqs (((Cond b ./\. inv ./\. veq) .=>. w)
                          ./\. ((anegate (Cond b) ./\. inv) .=>. q))
              (inv' <> bs' <> var'')
  --- Fix the bound variables and resolve them
  env <- ask
  let env' = foldr (\(x,y) a -> M.insert x y a) env fas
  inner' <- local (const env') $ resolveQ inner
  let fas' = foldr (Forall . snd) inner' fas
  return $ inv ./\. fas'
  where
    resolveVar :: Variant -> WP (FOL -> FOL, FOL, FOL)
    resolveVar v = do
      x <- genVar "variant"
      let cs' = Cond (bnegate (RBinary Greater (IntConst 0) (Var x))) ./\.
               Cond (RBinary Less v (Var x))
      return (Forall x, cs', Cond (RBinary Eq (Var x) v))

genGhost :: VName -> WP ()
genGhost x = do
  s <- get
  case M.lookup x s of
    Nothing -> modify (M.insert x 0)
    Just _ -> lift $ lift $ Left $ "👻-Variable " <> x <> " is already declared!!!"

genVar :: VName -> WP VName
genVar x = do
  s <- get
  let c = fromMaybe 1 (M.lookup x s)
      c' = x <> "#" <> show c
  modify (M.insert x (c+1))
  return c'

resolveQ :: FOL -> WP FOL
resolveQ (Cond b) = resolveBExpr b
resolveQ (Forall x q) = Forall x <$> resolveQ q
resolveQ (Exists x q) = Exists x <$> resolveQ q
resolveQ (ANegate q) = anegate <$> resolveQ q
resolveQ (AConj a b) = onlM2 aconj resolveQ a b
resolveQ (ADisj a b) = onlM2 adisj resolveQ a b
resolveQ (AImp a b) = onlM2 aimp resolveQ a b

resolveBExpr :: BExpr -> WP FOL
resolveBExpr b@(BoolConst _) = return $ Cond b
resolveBExpr (Negate b) = anegate <$> resolveBExpr b
resolveBExpr (BBinary Conj a b) = onlM2 aconj resolveBExpr a b
resolveBExpr (BBinary Disj a b) = onlM2 adisj resolveBExpr a b
resolveBExpr (RBinary op a b) = Cond <$> onlM2 (RBinary op) resolveAExpr a b

fixFOL :: FOL -> WP [FOL -> FOL]
fixFOL (Cond b) = fixBExpr b
fixFOL (Forall _ q) = fixFOL q
fixFOL (Exists _ q) = fixFOL q
fixFOL (ANegate q) = fixFOL q
fixFOL (AConj a b) = onlM2 (<>) fixFOL a b
fixFOL (ADisj a b) = onlM2 (<>) fixFOL a b
fixFOL (AImp a b) = onlM2 (<>) fixFOL a b

fixBExpr :: BExpr -> WP [FOL -> FOL]
fixBExpr (BoolConst _) = return []
fixBExpr (Negate b) = fixBExpr b
fixBExpr (BBinary _ a b) = onlM2 (<>) fixBExpr a b
fixBExpr (RBinary _ a b) = onlM2 (<>) fixAExpr a b

fixAExpr :: AExpr -> WP [FOL -> FOL]
fixAExpr (Var _) = return []
fixAExpr (Ghost _) = return []
fixAExpr (IntConst _) = return []
fixAExpr (Neg a) = fixAExpr a
fixAExpr (ABinary Div a b) = do
  a' <- fixAExpr a
  b' <- fixAExpr b
  return $ by0check (a' <> b') b
fixAExpr (ABinary Mod a b) = do -- Remember its Left Assoc
  a' <- fixAExpr a
  b' <- fixAExpr b
  return $ by0check (a' <> b') b
fixAExpr (ABinary _ a b) = do
  a' <- fixAExpr a
  b' <- fixAExpr b
  return $ a'<> b'

by0check ::  [FOL -> FOL] -> AExpr -> [FOL -> FOL]
by0check a b =  a <> [\x -> anegate (Cond (eq b (IntConst 0)))
                       ./\. (Cond (bnegate (eq b (IntConst 0))) .=>. x)]

reqs :: Foldable t1 => t2 -> t1 (t2 -> t2) -> t2
reqs = foldr (\o acc -> o acc)

resolveAExpr :: AExpr -> WP AExpr
resolveAExpr (Var x) = Var <$> mrVar1 x
resolveAExpr (Ghost x) = Ghost <$> mrVar1 x
resolveAExpr i@(IntConst _) = return i
resolveAExpr (Neg a) = Neg <$> resolveAExpr a
resolveAExpr (ABinary op a b) = onlM2 (abinary op) resolveAExpr a b

mrVar1 :: VName -> WP VName
mrVar1 x =
  if "#" `isInfixOf` x then
    return x
  else do
    s <- ask
    case M.lookup x s of
      Just x' -> return x'
      Nothing -> return x

findVars :: Stmt -> [(VName,VName)] -> WP [(VName,VName)]
findVars (Seq s1 s2) st = findVars s1 st >>= findVars s2
findVars (Assign x _) st = if x `elem` map fst st then
                             return st
                           else genVar x >>= \y -> return ((x,y):st)
findVars (Asst a) st = findInAsst a st
findVars (If _ s1 s2) st = findVars s1 st >>= findVars s2
findVars (While _ _ _ s) st = findVars s st
findVars _ st = return st

findInAsst :: FOL -> [(VName,VName)] -> WP [(VName,VName)]
findInAsst (Cond _) st = return st
findInAsst (Forall x a) st = genVar x >>= \y -> findInAsst a ((x,y):st)
findInAsst (Exists x a) st = genVar x >>= \y -> findInAsst a ((x,y):st)
findInAsst (ANegate a) st = findInAsst a st
findInAsst (AConj a b) st = findInAsst a st >>= findInAsst b
findInAsst (ADisj a b) st = findInAsst a st >>= findInAsst b
findInAsst (AImp a b) st = findInAsst a st >>= findInAsst b

