{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.RWS
import Data.Functor
import qualified Data.Map.Strict as M
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

update :: VName -> AExpr -> Eval ()
update vname a = modify . M.insert vname =<< evalAExpr a

eval :: Stmt -> Eval ()
eval (Seq s1 s2) = eval s1 >> eval s2
eval (GhostAss vname a) = get >>= maybe (update vname a) (const e) . M.lookup vname
  where e = err $ "Ghost " <> vname <> " defined multiple times"
eval (Assign vname a) = update vname a
eval (If c s1 s2) = evalBExpr c >>= \c' -> if c' then eval s1 else eval s2
eval (Asst f) = evalFOL f >>= \case
  True  -> return ();
  False -> err $ "Assertion " <> prettyF f 0 <> " Failed"
eval w@(While c invs _var s) =
  evalFOL invs >>= \case
    False -> err . (msg <>) . show =<< get
    True -> evalBExpr c >>= \case True -> eval s >> eval w; False -> return ()
    where msg = "invariant " <> prettyF invs 0 <> " does not hold, with store "
eval Skip = return ()
eval Fail = err "A violation has happened"

evalFOL :: FOL -> Eval Bool
evalFOL (Cond b) = evalBExpr b
evalFOL (Forall _ _) =  return True
evalFOL (Exists _ _) = return True
evalFOL (ANegate a) = not <$> evalFOL a
evalFOL (AConj a b) = onlM2 (&&) evalFOL a b
evalFOL (ADisj a b) = onlM2 (||) evalFOL a b
evalFOL (AImp a b) = liftM2 (not ... (&&)) (evalFOL a) (not <$> evalFOL b)

-- updateEnv :: VName -> Integer -> STEnv -> STEnv
-- updateEnv = M.insert

evalAExpr :: AExpr -> Eval Integer
evalAExpr (IntConst i) = return i
evalAExpr (Var vname) = get >>= (\case
  Nothing -> err $ "Variable " <> vname <> " not found"
  Just a -> return a) . M.lookup vname
evalAExpr (Ghost vname) = get >>= (\case
    Nothing -> err $ "Variable " <> vname <> " not found"
    Just a -> return a) . M.lookup vname
evalAExpr (Neg a) = evalAExpr a <&> negate
evalAExpr (ABinary op a1 a2) =
  onlM2 (evalAOp op) evalAExpr a1 a2 >>= \case Left e -> err e; Right r -> return r

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

err :: String -> Eval a
err = lift . Left
