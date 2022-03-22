{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module WLP where

import Data.SBV.Trans
import Control.Monad(liftM2)
import qualified Data.Map as M
import Control.Monad.State.Strict
import AST
import Eval
import GHC.TypeLits (Symbol)
import Control.Monad.Reader
import Data.Maybe
import Data.List (isInfixOf, isPrefixOf)
import Data.Function (on)
import Control.Monad.RWS (modify)
import Data.Bifunctor (first)
import Pretty
import Data.Foldable (foldrM)
import Control.Monad.Except
import Utils

type Sym a = SymbolicT (ExceptT String IO) a
type SymTable = M.Map VName SInteger

type Count = Int
type Counter = M.Map VName Count
type Env = M.Map VName VName

type WP a = StateT Counter (ReaderT Env (Either String)) a

runWLP s (vs, pre) = do
  runReaderT (runStateT addPre M.empty) M.empty
  where
    addPre = do
      wlp' <- wlp s $ Cond (BoolConst True)
      pre' <- case pre of
                Nothing -> return $ wlp'
                Just a -> return $ a .=>. wlp'
      return $ foldr Forall pre' vs

proveWLP s st =
  case runWLP s st of
    Right (q', _) -> return $ fToS q' M.empty
    Left e -> Left e

genGhost :: VName -> WP ()
genGhost x = do
  s <- get
  case M.lookup x s of
    Nothing -> modify (M.insert x 0)
    Just a -> lift $ lift $ Left $ "👻-Variable " <> x <> " is already declared!!!"

genVar :: VName -> WP VName
genVar x = do
  s <- get
  let c = fromMaybe 1 (M.lookup x s)
      c' = x <> "#" <> show c
  modify (M.insert x (c+1))
  return c'

false :: WP FOL
false = return . Cond . BoolConst $ False

resolveQ1 :: FOL -> WP FOL
resolveQ1 (Cond b) = resolveBExpr1 b
resolveQ1 (Forall x q) = Forall x <$> resolveQ1 q
resolveQ1 (Exists x q) = Exists x <$> resolveQ1 q
resolveQ1 (ANegate q) = anegate <$> resolveQ1 q
resolveQ1 (AConj a b) = onlM2 aconj resolveQ1 a b
resolveQ1 (ADisj a b) = onlM2 adisj resolveQ1 a b
resolveQ1 (AImp a b) = onlM2 aimp resolveQ1 a b

resolveBExpr1 :: BExpr -> WP FOL
resolveBExpr1 b@(BoolConst _) = return $ Cond b
resolveBExpr1 (Negate b) = anegate <$> resolveBExpr1 b
resolveBExpr1 (BBinary Conj a b) = onlM2 aconj resolveBExpr1 a b
resolveBExpr1 (BBinary Disj a b) = onlM2 adisj resolveBExpr1 a b
resolveBExpr1 (RBinary op a b) = Cond <$> onlM2 (RBinary op) resolveAExpr1 a b

resolveAExpr1 :: AExpr -> WP AExpr
resolveAExpr1 (Var x) = Var <$> mrVar1 x
resolveAExpr1 a@(Ghost _) = return a
resolveAExpr1 i@(IntConst _) = return i
resolveAExpr1 (Neg a) = Neg <$> resolveAExpr1 a
resolveAExpr1 (ABinary Div a b) = do
  b' <- resolveAExpr1 b
  if b' /= IntConst 0 then flip (abinary Div) b' <$> resolveAExpr1 a
  else lift . lift . Left $ "Division by 0"
resolveAExpr1 (ABinary Mod a b) = do
  b' <- resolveAExpr1 b
  if b' /= IntConst 0 then flip (abinary Div) b' <$> resolveAExpr1 a
  else lift . lift . Left $ "Modulo by 0"
resolveAExpr1 (ABinary op a b) = onlM2 (abinary op) resolveAExpr1 a b

wlp :: Stmt -> FOL -> WP FOL
wlp (Seq s1 s2) q = wlp s2 q >>= wlp s1
wlp Skip q = return q
wlp (GhostAss x a) q =
  genGhost x >> resolveQ1 (Forall x (Cond (RBinary Eq (Ghost x) a) .=>. q))
wlp (Assign x a) q = do
    x' <- genVar x
    q' <- local (M.insert x x') $ resolveQ1 q
    return $ Forall x' (Cond (RBinary Eq (Var x') a) .=>. q')
wlp (If b s1 s2) q = do
  s1' <- wlp s1 q
  s2' <- wlp s2 q
  let b' = Cond b
  return $ (b' .=>. s1') ./\. (anegate b' .=>. s2')
wlp s@(Asst a) q =
  findVars s [] >> return (a ./\. q)
wlp Fail _q = false
wlp (While b [] _var _s) q = lift . lift . Left $ "while with condition "
                             <> prettyB b <> " has no invariant!"
wlp (While b inv var s) q = do
  let invs = foldr1 (./\.) inv   -- Foldr all invariants
  st <- get
  -- Give back condition for unbounded integer variant
  (fa, var', veq) <- maybe (return (id,
                                    Cond $ BoolConst True,
                                    Cond $ BoolConst True)) resolveVar var
  -- wlp(s, I /\ invariant condition)
  w <- wlp s (invs ./\. var')
  -- check which variables we wanna forall over
  fas <- findVars s []
  let inner = fa (((Cond b ./\. invs ./\. veq) .=>. w)
                          ./\. ((anegate (Cond b) ./\. invs) .=>. q))
  --- Fix the bound variables and resolve them
  env <- ask
  let env' = foldr (\(x,y) a -> M.insert x y a) env fas
  inner' <- local (const env') $ resolveQ1 inner
  let fas' = foldr (Forall . snd) inner' fas
  return $ invs ./\. fas'
  where
    resolveVar :: Variant -> WP (FOL -> FOL, FOL, FOL)
    resolveVar var = do
      x <- genVar "variant"
      return (Forall x,
               Cond (Negate (RBinary Greater (IntConst 0) (Var x))) ./\.
               Cond (RBinary Less var (Var x))
             , Cond (RBinary Eq (Var x) var)
             )

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
findVars (Assign x a) st = if x `elem` map fst st then
                             return st
                           else genVar x >>= \y -> return ((x,y):st)
findVars (Asst a) st = findInAsst a st
findVars (If _ s1 s2) st = findVars s1 st >>= findVars s2
findVars (While _ _ _ s) st = findVars s st
findVars _ st = return st

findInAsst :: FOL -> [(VName,VName)] -> WP [(VName,VName)]
findInAsst (Cond b) st = return st
findInAsst (Forall x a) st = genVar x >>= \y -> findInAsst a ((x,y):st)
findInAsst (Exists x a) st = genVar x >>= \y -> findInAsst a ((x,y):st)
findInAsst (ANegate a) st = findInAsst a st
findInAsst (AConj a b) st = findInAsst a st >>= findInAsst b
findInAsst (ADisj a b) st = findInAsst a st >>= findInAsst b
findInAsst (AImp a b) st = findInAsst a st >>= findInAsst b

fToS :: FOL -> SymTable -> Sym SBool
fToS (Cond b) st = bToS b st
fToS (Forall x a) st = forAll [x] $ \(x'::SInteger) ->
  fToS a (M.insert x x' st)
fToS (Exists x a) st = forSome [x] $ \(x'::SInteger) ->
  fToS a (M.insert x x' st)
fToS (ANegate a) st = sNot <$> fToS a st
fToS (AConj a b) st = onlM2 (.&&) (`fToS` st) a b
fToS (ADisj a b) st = onlM2 (.||) (`fToS` st) a b
fToS (AImp a b) st = onlM2 (.=>) (`fToS` st) a b

bToS :: BExpr -> SymTable -> Sym SBool
bToS (BoolConst b) st = return $ fromBool b
bToS (Negate b) st = sNot <$> bToS b st
bToS (BBinary op a b) st = onlM2 (f op) (`bToS` st) a b
  where f Conj = (.&&)
        f Disj = (.||)
bToS (RBinary op a b) st = onlM2 (f op) (`aToS` st) a b
  where f Less = (.<)
        f Eq   = (.==)
        f Greater = (.>)

aToS :: AExpr -> SymTable -> Sym SInteger
aToS (Var x) st =
  case M.lookup x st of
    Just a -> return a
    Nothing -> throwError $ "Var " <> x <> " Not found in " <> show st
aToS (Ghost x) st =
  case M.lookup x st of
    Just a -> return a
    Nothing -> throwError $ "Var " <> x <> " Not found in " <> show st
aToS (IntConst i) st = return $ literal i
aToS (Neg a) st = negate <$> aToS a st
aToS (ABinary op a b) st = onlM2 (f op) (`aToS` st) a b
  where f Add = (+)
        f Sub = (-)
        f Mul = (*)
        f Div = sDiv
        f Mod = sMod
