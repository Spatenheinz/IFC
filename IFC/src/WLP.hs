{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WLP where

import Data.SBV
import Control.Monad(liftM2)
import qualified Data.Map as M
import Control.Monad.State.Strict
import AST
import Eval
import GHC.TypeLits (Symbol)
import AST (BExpr (BBinary, BoolConst, Negate), AExpr (IntConst), FOL (ANegate, AConj))
import Control.Monad.State.Strict (StateT(runStateT))
import Data.Maybe
import Data.List (isInfixOf)
import Data.Function (on)
import Control.Monad.RWS (modify)
import Data.Bifunctor (first)

type Sym a = StateT SymTable Symbolic (SBV a)
type SymTable = M.Map String SInteger

type Counter = Int
type Store = M.Map String ([(VName, AExpr)], Counter)

type Formular a = StateT Store (Either String) a

runWLP st s = runStateT (preP s (Cond $ BoolConst True) >>=
                         \x -> formatState >> resolveQ x) $ M.fromList st

proveWLP st s = let q = runWLP st s
                in case q of
                     Right (q', _) -> return $ fToS q' M.empty
                     Left e -> Left e

formatState :: Formular ()
formatState = modify (M.map (first (("#", Var "") :)))

genVar :: VName -> AExpr -> Formular VName
genVar x a = do
  s <- get
  let (as,c) = fromMaybe ([],1) (M.lookup x s)
      c' = x <> "#" <> show c
  modify (M.insert x ((c',a):as,c+1))
  return c'

false = return . Cond . BoolConst $ False

preP :: Stmt -> FOL -> Formular FOL
preP (Seq s1 s2) q = preP s2 q >>= preP s1
preP Skip q = return q
preP (Assign x a) q = do
  x' <- genVar x a
  return $ Forall x' ( Cond (RBinary Eq (Var x') a) .=>. q)
preP (If b s1 s2) q = do
  s1' <- preP s1 q
  s2' <- preP s2 q
  let b' = Cond b
  return $ b' ./\. s1' .\/. ANegate b' ./\. s2'
preP (Asst a) q = do
  return $ a ./\. q
preP Fail _q = false
preP (While _b [] _var _s) q = false
preP (While _b inv _var _s) q = return $ foldr1 (./\.) inv

resolveQ :: FOL -> Formular FOL
resolveQ (Cond b) = resolveBExpr b
resolveQ (Forall x q) = popStack x >> Forall x <$> resolveQ q
resolveQ (Exists x q) = popStack x >> Forall x <$> resolveQ q
resolveQ (ANegate q) = ANegate <$> resolveQ q
resolveQ (AConj a b) = liftM2 AConj (resolveQ a) (resolveQ b)
resolveQ (ADisj a b) = liftM2 ADisj (resolveQ a) (resolveQ b)
resolveQ (AImp a b) = liftM2 AImp (resolveQ a) (resolveQ b)

popStack :: VName -> Formular ()
popStack x =
  when ("#" `isInfixOf` x) (do
    s <- get
    let x' = takeWhile (/='#') x
    modify (M.adjust (\(a:as, c) -> (as,c)) x')
    )

resolveBExpr :: BExpr -> Formular FOL
resolveBExpr b@(BoolConst _) = return $ Cond b
resolveBExpr (Negate b) = ANegate <$> resolveBExpr b
resolveBExpr (BBinary Conj a b) = on (liftM2 AConj) resolveBExpr a b
resolveBExpr (BBinary Disj a b) = on (liftM2 ADisj) resolveBExpr a b
resolveBExpr (RBinary op a b) = Cond <$> on (liftM2 (RBinary op)) resolveAExpr a b

resolveAExpr :: AExpr -> Formular AExpr
resolveAExpr (Var x) = Var <$> mrVar x
resolveAExpr (Ghost x) = Var <$> mrVar x
resolveAExpr i@(IntConst _) = return i
resolveAExpr (Neg a) = Neg <$> resolveAExpr a
resolveAExpr (ABinary op a b) = on (liftM2 (ABinary op)) resolveAExpr a b

mrVar :: VName -> Formular VName
mrVar x = do
  if "#" `isInfixOf` x then
    return x
  else do
    s <- get
    case M.lookup x s of
      Just ([],c) -> error "COMPILER ERROR"
      Just ((x',a):as,c) -> return x'
      Nothing -> lift . Left $ "variable " <> x <> " not declared"

fToS :: FOL -> SymTable -> Predicate
fToS (Cond b) st = bToS b st
fToS (Forall x a) st = forAll [x] $ \(x'::SInteger) ->
  fToS a (M.insert x x' st)
fToS (Exists x a) st = forSome [x] $ \(x'::SInteger) ->
  fToS a (M.insert x x' st)
fToS (ANegate a) st = sNot <$> fToS a st
fToS (AConj a b) st = on (liftM2 (.&&)) (`fToS` st) a b
fToS (ADisj a b) st = on (liftM2 (.||)) (`fToS` st) a b
fToS (AImp a b) st = on (liftM2 (.=>)) (`fToS` st) a b

bToS :: BExpr -> SymTable -> Predicate
bToS (BoolConst b) st = return $ fromBool b
bToS (Negate b) st = sNot <$> bToS b st
bToS (BBinary op a b) st = on (liftM2 (f op)) (`bToS` st) a b
  where f Conj = (.&&)
        f Disj = (.||)
bToS (RBinary op a b) st = on (liftM2 (f op)) (`aToS` st) a b
  where f Less = (.<)
        f Eq   = (.==)
        f Greater = (.>)

aToS :: AExpr -> SymTable -> Symbolic SInteger
aToS (Var x) st = do
  case M.lookup x st of
    Just a -> return a
    Nothing -> error $ "Var " <> x <> " Not found in " <> show st
aToS (Ghost x) st = undefined
aToS (IntConst i) st = return $ literal i
aToS (Neg a) st = negate <$> aToS a st
aToS (ABinary op a b) st = on (liftM2 (f op)) (`aToS` st) a b
  where f Add = (+)
        f Sub = (-)
        f Mul = (*)
        f Div = sDiv
        f Mod = sMod
-- fToS :: FOL -> Sym Bool
-- fToS (Cond b) = bToS b
-- fToS (Forall x a) = lift $ forAll [x] $ \(x'::SInteger) ->
--   modify (M.insert x x') >> fToS a
-- fToS (Exists x a) = lift $ forSome [x] $ \(x'::SInteger) ->
--   modify (M.insert x x') >> fToS a
-- fToS (ANegate a) = sNot <$> fToS a
-- fToS (AConj a b) = on (liftM2 (.&&)) fToS a b
-- fToS (ADisj a b) = on (liftM2 (.||)) fToS a b
-- fToS (AImp a b) = on (liftM2 (.=>)) fToS a b

-- bToS :: BExpr -> Sym Bool
-- bToS (BoolConst b) = return $ fromBool b
-- bToS (Negate b) = sNot <$> bToS b
-- bToS (BBinary op a b) = on (liftM2 (f op)) bToS a b
--   where f Conj = (.&&)
--         f Disj = (.||)
-- bToS (RBinary op a b) = on (liftM2 (f op)) aToS a b
--   where f Less = (.<)
--         f Eq   = (.==)
--         f Greater = (.>)

-- aToS :: AExpr -> Sym Integer
-- aToS (Var x) = do
--   s <- get
--   case M.lookup x s of
--     Just a -> return a
--     Nothing -> error "Should not be possible"
-- aToS (Ghost x) = undefined
-- aToS (IntConst i) = return $ literal i
-- aToS (Neg a) = negate <$> aToS a
-- aToS (ABinary op a b) = on (liftM2 (f op)) aToS a b
--   where f Add = (+)
--         f Sub = (-)
--         f Mul = (*)
--         f Div = sDiv
--         f Mod = sMod
-- wlp :: Stmt -> SBool -> Sym Bool
-- wlp (Seq s1 s2) q = wlp s2 q >>= wlp s1
-- wlp Skip q = return q
-- wlp (Assign x a) q = do
--   a' <- resolveAExpr a
--   forAll_ $ \(y :: SInteger) -> a' .== y .=> resolveQ y q
-- wlp (If b s1 s2) q =
--   let b' = resolveBExpr b
--   in do s1' <- wlp s1 q
--         s2' <- wlp s2 q
--         return $ ite b' s1' s2'
--   -- return $ b' .&& s1' .|| sNot b' .&& s2'
-- wlp (Asst a) q = do
--   a' <- resolveFOL a
--   return $ a' .&& q
-- wlp Fail _q = return sFalse
-- wlp (While _b [] _var _s) q = return sFalse
-- wlp (While _b inv _var _s) q = foldr1 (.&&) <$> mapM resolveFOL inv

-- vc :: Stmt -> SBool -> Sym Bool
-- vc (Seq s1 s2) q = do
--   vcs <- vc s2 q
--   s1' <- wlp s2 q >>= vc s1
--   return $ s1' .&& vcs
-- vc Skip q = return sTrue
-- vc (Assign _ _) q = return sTrue
-- vc (If b s1 s2) q = do
--   s1' <- vc s1 q
--   s2' <- vc s2 q
--   return $ s1' .&& s2'
-- vc (Asst f) q = return sTrue
-- vc (While b [] var s) q = return sFalse
-- vc (While b inv var s) q = do
--   let b' = resolveBExpr b
--   invs <- foldr1 (.&&) <$> mapM resolveFOL inv
--   weaks <- wlp s invs
--   vcs <- vc s invs
--   return $ (b' .&& invs .=> weaks) .&& (sNot b' .&& invs .=> q) .&& vcs
-- vc Fail q = return sFalse

-- resolveFOL :: FOL -> Sym Bool
-- resolveFOL (Cond b) y = return $ resolveBExpr b y
-- resolveFOL (Forall x f) = forAll_ $ \(y::SInteger) -> resolveFOL f y
-- resolveFOL (ANegate f) = sNot <$> resolveFOL f
-- resolveFOL (AConj f1 f2) = do
--   f1' <- resolveFOL f1
--   f2' <- resolveFOL f2
--   return $ f1' .&& f2'

-- resolveBExpr :: BExpr -> SBool
-- resolveBExpr (BoolConst b) = fromBool b
-- resolveBExpr (Negate b) = sNot $ resolveBExpr b
-- resolveBExpr (BBinary op b1 b2) =
--   let b1' = resolveBExpr b1
--       b2' = resolveBExpr b2
--   in f op b1' b2'
--   where f Conj = (.&&)
--         f Disj = (.||)
-- resolveBExpr (RBinary op a1 a2) =
--   let a1' = resolveAExpr a1
--       a2' = resolveAExpr a2
--   in f op a1' a2'
--   where f Less = (.<)
--         f Eq   = (.==)
--         f Greater = (.>)

-- resolveAExpr :: AExpr -> Store -> SInteger
-- resolveAExpr (Var x) sto = case M.lookup x sto of
--                              Just a -> a
--                              Nothing -> error "grumpy"
-- resolveAExpr (Ghost x) sto = undefined
-- resolveAExpr (IntConst i) _ = literal i
-- resolveAExpr (Neg a) sto = negate $ resolveAExpr a sto
-- resolveAExpr (ABinary op a1 a2) sto =
--   let a1' = resolveAExpr a1 sto
--       a2' = resolveAExpr a2 sto
--   in f op a1' a2'
--   where f Add = (+)
--         f Sub = (-)
--         f Mul = (*)
--         f Div = sDiv
--         f Mod = sMod

-- resolveQ :: SInteger -> Sym SBool

-- -- resolve :: SymVal a => VName -> Symbolic (SBV a) -> WLP (Symbolic (SBV a))
-- -- resolve x q =
