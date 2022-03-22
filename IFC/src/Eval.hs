{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.RWS
import Data.Functor
import qualified Data.Map.Strict as M
import Data.Function (on)
import System.IO (putStrLn)
import AST
import Pretty
import Utils

type STEnv = M.Map VName Integer
type Err a = Either String a
type Eval a =
  RWST ()               -- We dont use Reader at the moment
       ()           -- Do we want to be able to print?
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
eval (GhostAss vname a) = do
  st <- get
  case M.lookup vname st of
    Just a -> lift . Left $ "Ghost " <> vname <> " defined multiple times"
    Nothing -> modify . updateEnv vname =<< evalAExpr a
eval (Assign vname a) = modify . updateEnv vname =<< evalAExpr a
eval (If c s1 s2) = do
  c' <- evalBExpr c
  if c' then eval s1 else eval s2
eval (Asst f) = evalFOL f >>= \case
  True -> return ()
  False -> lift . Left $ "Assertion " <> prettyF f 0 <> " Failed"
eval w@(While c invs var s) = do
  c' <- evalBExpr c
  invs' <- evalFOL (foldr1 (./\.) invs)
  if c' && invs' then (eval s >> eval w)
  else if invs' then (return ())
  else do
      st <- get
      lift. Left $ "invariant " <> prettyF (foldr1 (./\.) invs) 0 <> " does not hold, with store " <> show st
eval Skip = return ()
eval Fail = lift $ Left "A violation has happened"

evalFOL :: FOL -> Eval Bool
evalFOL (Cond b) = evalBExpr b
evalFOL (Forall _ _) =  return True --lift $ Left "Forall are currently unsupported"
evalFOL (Exists _ _) = return True --lift $ Left "Exists are current unsupported"
evalFOL (ANegate a) = not <$> evalFOL a
evalFOL (AConj a b) = onlM2 (&&) evalFOL a b
evalFOL (ADisj a b) = onlM2 (||) evalFOL a b
evalFOL (AImp a b) = liftM2 (not ... (&&)) (evalFOL a) (not <$> evalFOL b)

updateEnv :: VName -> Integer -> STEnv -> STEnv
updateEnv = M.insert

evalAExpr :: AExpr -> Eval Integer
evalAExpr (IntConst i) = return i
evalAExpr (Var vname) = get >>= (\case
  Nothing -> lift $ Left $ "Variable " <> vname <> " not found"
  Just a -> return a) . M.lookup vname
evalAExpr (Ghost vname) = get >>= (\case
    Nothing -> lift $ Left $ "Variable " <> vname <> " not found"
    Just a -> return a) . M.lookup vname
evalAExpr (Neg a) = evalAExpr a <&> negate
evalAExpr (ABinary op a1 a2) = onlM2 (evalAOp op) evalAExpr a1 a2 >>= \case
    Left e -> lift $ Left e
    Right r -> return r

evalAOp :: ArithOp -> Integer -> Integer -> Err Integer
evalAOp Div _ 0  = Left "Division by 0"
evalAOp Mod _ 0 = Left "Modulo by 0"
evalAOp op a b =
  case lookup op binops of
    Nothing -> error "operator not supported"
    Just x -> return ( a `x` b)

binops :: (Num a, Integral a) => [(ArithOp, a -> a -> a)]
binops = [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div), (Mod, mod)]

evalBExpr :: BExpr -> Eval Bool
evalBExpr (BoolConst b) = return b
evalBExpr (Negate b) = evalBExpr b <&> not
evalBExpr (BBinary op b1 b2) = onlM2 (evalBOp op) evalBExpr b1 b2
evalBExpr (RBinary op a1 a2) = onlM2 (evalROp op) evalAExpr a1 a2

evalBOp :: BoolOp -> Bool -> Bool -> Bool
evalBOp Conj a b = a && b
evalBOp Disj a b = a || b

evalROp :: ROp -> Integer -> Integer -> Bool
evalROp Less a b = a < b
evalROp Eq a b = a == b
evalROp Greater a b = a > b
