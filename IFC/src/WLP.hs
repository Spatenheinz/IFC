{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module WLP where

import Data.SBV
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
import Debug.Trace
import Pretty

type Sym a = StateT SymTable Symbolic (SBV a)
type SymTable = M.Map String SInteger

type Counter = Int
type Store = M.Map String ([VName], Counter)

type Formular a = ReaderT Store (Either String) a
type PreProcess a = StateT Store (Either String) a

runWLP st s = case runStateT (wlpAndvc s) $ M.fromList st of
                Right (q, s) -> case runReaderT (resolveQ q) s of
                                  Right q' -> return  (q', s)
                                  Left e -> Left e
                Left e -> Left e

wlpAndvc s = do
  w <- wlp s (Cond $ BoolConst True)
  formatState
  return $ w

proveWLP st s =
  case runWLP st s of
    Right (q', _) -> return $ fToS q' M.empty
    Left e -> Left e

formatState :: PreProcess ()
formatState = modify (M.mapWithKey (\k (x,c) -> (k:x,c)))

genGhost :: VName -> PreProcess ()
genGhost x = do
  s <- get
  case M.lookup x s of
    Nothing -> modify (M.insert x ([x],0))
    Just a -> lift $ Left $ "👻-Variable " <> x <> " is already declared!!!"

genVar :: VName -> PreProcess VName
genVar x = do
  s <- get
  let (as,c) = fromMaybe ([],1) (M.lookup x s)
      c' = x <> "#" <> show c
  modify (M.insert x (c':as,c+1))
  return c'

false :: PreProcess FOL
false = return . Cond . BoolConst $ False

wlp :: Stmt -> FOL -> PreProcess FOL
wlp (Seq s1 s2) q = wlp s2 q >>= wlp s1
wlp Skip q = return q
wlp (GhostAss x a) q =
  genGhost x >> return (Forall x (Cond (RBinary Eq (Ghost x) a) .=>. q))
wlp (Assign x a) q = do
  x' <- genVar x 
  return $ Forall x' (Cond (RBinary Eq (Var x') a) .=>. q)
wlp (If b s1 s2) q = do
  s1' <- wlp s1 q
  s2' <- wlp s2 q
  let b' = Cond b
  return $ b' ./\. s1' .\/. ANegate b' ./\. s2'
wlp s@(Asst a) q =
  findVars s [] >> (return $ a ./\. q)
wlp Fail _q = false
wlp (While _b [] _var _s) q = false
wlp (While b inv var s) q = do
  let invs = foldr1 (./\.) inv
  st <- get
  (fa, var') <- maybe (return (id, invs)) resolveVar var
  w <- wlp s var'
  fas <- findVars s []
  let inner = fa ((Cond b ./\. (w .\/. ANegate invs))
                          ./\. ANegate (Cond b) ./\. invs .=>. q)
  let fas' = foldr Forall inner fas
  return $ invs ./\. fas'
  where
    resolveVar :: Variant -> PreProcess (FOL -> FOL, FOL)
    resolveVar var = do
      x <- genVar "variant"
      return (Forall x,
               Cond (Negate (RBinary Greater (IntConst 0) var)) ./\.
               Cond (RBinary Less (Var x) var))

findVars :: Stmt -> [VName] -> PreProcess [VName]
findVars (Seq s1 s2) st = findVars s1 st >>= findVars s2
findVars (Assign x a) st = genVar x >>= \y -> return (y:st)
findVars (Asst a) st = findInAsst a st
findVars (If _ s1 s2) st = findVars s1 st >>= findVars s2
findVars (While _ _ _ s) st = findVars s st
findVars _ st = return st

findInAsst :: FOL -> [VName] -> PreProcess [VName]
findInAsst (Cond b) st = return st
findInAsst (Forall x a) st = genVar x >>= \y -> findInAsst a (y:st)
findInAsst (Exists x a) st = genVar x >>= \y -> findInAsst a (y:st)
findInAsst (ANegate a) st = findInAsst a st
findInAsst (AConj a b) st = findInAsst a st >>= findInAsst b
findInAsst (ADisj a b) st = findInAsst a st >>= findInAsst b
findInAsst (AImp a b) st = findInAsst a st >>= findInAsst b

resolveQ :: FOL -> Formular FOL
resolveQ (Cond b) = resolveBExpr b
resolveQ (Forall x q) = local (popStack x) $ Forall x <$> resolveQ q
resolveQ (Exists x q) = local (popStack x) $ Exists x <$> resolveQ q
resolveQ (ANegate q) = ANegate <$> resolveQ q
resolveQ (AConj a b) = liftM2 AConj (resolveQ a) (resolveQ b)
resolveQ (ADisj a b) = liftM2 ADisj (resolveQ a) (resolveQ b)
resolveQ (AImp a b) = liftM2 AImp (resolveQ a) (resolveQ b)


popStack :: VName -> Store -> Store
popStack x st = if
  | "👻" `isPrefixOf` x -> st
  | "#" `isInfixOf` x ->
    let x' = takeWhile (/='#') x in
    M.adjust (\(a:as, c) -> (as,c)) x' st
  | otherwise -> st

resolveBExpr :: BExpr -> Formular FOL
resolveBExpr b@(BoolConst _) = return $ Cond b
resolveBExpr (Negate b) = ANegate <$> resolveBExpr b
resolveBExpr (BBinary Conj a b) = on (liftM2 AConj) resolveBExpr a b
resolveBExpr (BBinary Disj a b) = on (liftM2 ADisj) resolveBExpr a b
resolveBExpr (RBinary op a b) = Cond <$> on (liftM2 (RBinary op)) resolveAExpr a b

resolveAExpr :: AExpr -> Formular AExpr
resolveAExpr (Var x) = Var <$> mrVar x
resolveAExpr (Ghost a) = return $ Var a
resolveAExpr i@(IntConst _) = return i
resolveAExpr (Neg a) = Neg <$> resolveAExpr a
resolveAExpr (ABinary op a b) = on (liftM2 (ABinary op)) resolveAExpr a b

mrVar :: VName -> Formular VName
mrVar x =
  if "#" `isInfixOf` x then
    return x
  else do
    s <- ask
    case M.lookup x s of
      Just ([],c) -> error "COMPILER ERROR"
      Just (x':as,c) -> return x'
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
aToS (Var x) st =
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

-- simplify :: FOL -> FOL
-- simplify b@(Cond _) = b
-- simplify (Forall x a) = Forall x $ simplify a
-- simplify (Exists x a) = Exists x $ simplify a
-- simplify (ANegate x) = x
-- simplify (AConj a b) | Cond (BoolConst True) <- simplify a = simplify b
--                      | Cond (BoolConst True) <- simplify b = simplify a
--                      | Cond (BoolConst False) <- simplify b = Cond (BoolConst False)
--                      | Cond (BoolConst False) <- simplify a = Cond (BoolConst False)
--                      | otherwise = on AConj simplify a b
-- simplify (ADisj a b) | Cond (BoolConst True) <- simplify a = Cond (BoolConst True)
--                      | Cond (BoolConst True) <- simplify b = Cond (BoolConst True)
--                      | Cond (BoolConst False) <- simplify b = simplify a
--                      | Cond (BoolConst True) <- simplify a = simplify b
--                      | otherwise = on ADisj simplify a b
-- simplify (AImp a b)  | Cond (BoolConst False) <- simplify a = Cond (BoolConst True)
--                      | Cond (BoolConst True) <- simplify a,
--                        Cond (BoolConst False) <- simplify b = Cond (BoolConst False)
--                      | otherwise = on AImp simplify a b
