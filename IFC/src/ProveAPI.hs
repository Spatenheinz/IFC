{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProveAPI where

import AST
import Control.Monad.Except
import Data.SBV.Trans
import qualified Data.Map as M
import Utils
import WP

type Sym a = SymbolicT (ExceptT String IO) a
type SymTable = M.Map VName SInteger

proveWLP :: Stmt -> Header -> Either String (Sym SBool)
proveWLP s st =
  case runWLP s st of
    Right (q', _) -> return $ fToS q' M.empty
    Left e -> Left e

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
bToS (BoolConst b) _st = return $ fromBool b
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
aToS (IntConst i) _st = return $ literal i
aToS (Neg a) st = negate <$> aToS a st
aToS (ABinary op a b) st = onlM2 (f op) (`aToS` st) a b
  where f Add = (+)
        f Sub = (-)
        f Mul = (*)
        f Div = sDiv
        f Mod = sMod
