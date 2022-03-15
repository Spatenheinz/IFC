{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.RWS
import Data.Functor
import qualified Data.Map.Strict as M
import System.IO (putStrLn)
import AST
import AST (AExpr(ABinary), ArithOp, BExpr (BoolConst), BoolOp)

type STEnv = M.Map VName Integer
type Err a = Either String a
data RunError = EBadVar VName | EBadArg String
type Eval a =
  RWST ()               -- We dont use Reader at the moment
       String           -- Do we want to be able to print?
       STEnv               -- Imperative stateful variables
       (Either String)
       a-- ...

printEval :: STEnv -> IO ()
printEval = mapM_ (\(v,x) -> putStrLn (v <> " " <> show x)) . M.toList

runEval :: [(VName, Integer)] -> Stmt -> Err STEnv
runEval xs ast =
  case execRWST (eval ast) () (M.fromList xs) of
    Left e -> Left e
    Right (stenv, _) -> return stenv

eval :: Stmt -> Eval ()
eval (Seq s1 s2) = eval s1 >> eval s2
eval (GhostAss vname a) = return ()
eval (Assign vname a) = evalAExpr a >>= \a' -> modify (updateEnv vname a')
eval (If c s1 s2) = do
  c' <- evalBExpr c
  if c' then eval s1 else eval s2
eval (Asst _) = return ()
eval (While c invs var s) =
  evalBExpr c >>= \c' -> when c' $ eval s >> eval (While c invs var s)
eval Skip = return ()
eval Fail = lift $ Left "you failed horribly"

updateEnv :: VName -> Integer -> STEnv -> STEnv
updateEnv = M.insert

evalAExpr :: AExpr -> Eval Integer
evalAExpr (IntConst i) = return i
evalAExpr (Var vname) = do
  st <- get
  case M.lookup vname st of
    Nothing -> lift $ Left $ "Variable " <> vname <> " not found"
    Just a -> return a
evalAExpr (Neg a) = evalAExpr a <&> negate
evalAExpr (ABinary op a1 a2) =
  liftM2 (evalAOp op) (evalAExpr a1) (evalAExpr a2) >>= \case
    Left e -> lift $ Left e
    Right r -> return r

evalAOp :: ArithOp -> Integer -> Integer -> Err Integer
evalAOp Div _ 0  = Left "Division by 0. Go home"
evalAOp Mod _ 0 = Left "Modulo by 0. Go home"
evalAOp op a b =
  case lookup op binops of
    Nothing -> error "operator not supported"
    Just x -> return ( a `x` b)

binops :: (Num a, Integral a) => [(ArithOp, a -> a -> a)]
binops = [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div), (Mod, mod)]

evalBExpr :: BExpr -> Eval Bool
evalBExpr (BoolConst b) = return b
evalBExpr (Negate b) = evalBExpr b <&> not
evalBExpr (BBinary op b1 b2) =
  liftM2 (evalBOp op) (evalBExpr b1) (evalBExpr b2)
evalBExpr (RBinary op a1 a2) =
  liftM2 (evalROp op) (evalAExpr a1) (evalAExpr a2)

evalBOp :: BoolOp -> Bool -> Bool -> Bool
evalBOp Conj a b = a && b
evalBOp Disj a b = a || b

evalROp :: ROp -> Integer -> Integer -> Bool
evalROp Less a b = a < b
evalROp Eq a b = a == b
evalROp Greater a b = a > b
