{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module WP where

import qualified Data.Map as M
import Control.Monad.State.Strict
import AST
-- import GHC.TypeLits (Symbol)
import Control.Monad.Reader
import Data.Maybe
import Data.List (isInfixOf)
import Utils


type Count = Int
type Counter = M.Map VName Count
type Env = M.Map VName VName

type WP a = StateT Counter (ReaderT Env (Either String)) a

runWLP :: Foldable t =>Stmt -> (t VName, Maybe FOL) -> Either String (FOL, Counter)
runWLP s (vs, pre) = do
  runReaderT (runStateT addPre M.empty) M.empty
  where
    addPre = do
      wlp' <- wlp s $ Cond (BoolConst True)
      pre' <- case pre of
                Nothing -> return $ wlp'
                Just a -> return $ a .=>. wlp'
      return $ foldr Forall pre' vs

wlp :: Stmt -> FOL -> WP FOL
wlp (Seq s1 s2) q = wlp s2 q >>= wlp s1
wlp Skip q = return q
wlp (GhostAss x a) q =
  genGhost x >> resolveQ (Forall x (Cond (RBinary Eq (Ghost x) a) .=>. q))
wlp (Assign x a) q = do
    x' <- genVar x
    q' <- local (M.insert x x') $ resolveQ q
    return $ Forall x' (Cond (RBinary Eq (Var x') a) .=>. q')
wlp (If b s1 s2) q = do
  s1' <- wlp s1 q
  s2' <- wlp s2 q
  let b' = Cond b
  return $ (b' .=>. s1') ./\. (anegate b' .=>. s2')
wlp s@(Asst a) q =
  findVars s [] >> return (a ./\. q)
wlp Fail _q = false
wlp (While b inv var s) q = do
  -- Give back condition for unbounded integer variant
  (fa, var', veq) <- maybe (return (id,
                                    Cond $ BoolConst True,
                                    Cond $ BoolConst True)) resolveVar var
  -- wlp(s, I /\ invariant condition)
  w <- wlp s (inv ./\. var')
  -- check which variables we wanna forall over
  fas <- findVars s []
  let inner = fa (((Cond b ./\. inv ./\. veq) .=>. w)
                          ./\. ((anegate (Cond b) ./\. inv) .=>. q))
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
      return (Forall x,
               Cond (Negate (RBinary Greater (IntConst 0) (Var x))) ./\.
               Cond (RBinary Less v (Var x))
             , Cond (RBinary Eq (Var x) v)
             )

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

false :: WP FOL
false = return . Cond . BoolConst $ False

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

resolveAExpr :: AExpr -> WP AExpr
resolveAExpr (Var x) = Var <$> mrVar1 x
resolveAExpr a@(Ghost _) = return a
resolveAExpr i@(IntConst _) = return i
resolveAExpr (Neg a) = Neg <$> resolveAExpr a
resolveAExpr (ABinary Div a b) = do
  b' <- resolveAExpr b
  if b' /= IntConst 0 then flip (abinary Div) b' <$> resolveAExpr a
  else lift . lift . Left $ "Division by 0"
resolveAExpr (ABinary Mod a b) = do
  b' <- resolveAExpr b
  if b' /= IntConst 0 then flip (abinary Div) b' <$> resolveAExpr a
  else lift . lift . Left $ "Modulo by 0"
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

