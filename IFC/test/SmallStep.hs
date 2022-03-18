{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SmallStep where
import Test.Tasty (testGroup)
import AST
import QCInstances
import CodeBlocks

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===), arbitrary)
import Eval
import qualified Data.Map as M
import Data.Either
import Control.Monad
import Control.Monad.RWS

equivalence = testGroup "Tests for equivalences" [
        testGroup "QC" [
            testProperty "Aexpr" $ \(aexpr :: AExpr) ->
                evalA [] aexpr === evalA [] aexpr
            , testProperty "Bexpr" $ \(bexpr :: BExpr) ->
                evalB [] bexpr === evalB [] bexpr
            , testProperty "Assign" $ \(i :: fixednames) (a :: AExpr) ->
                case evalA [] a of
                  Left e -> vacous
                  Right a' -> runEval [] (Assign i a ) === Right (M.fromList [(i, a')])
            , testProperty "Assign2" $ \(i :: fixednames) (a :: AExpr) ->
                case evalA [] a of
                  Left e -> vacous
                  Right a' -> runEval [] (Assign i a) === runEval [] (Assign i (IntConst a'))
            , testProperty "AssignSkip" $ \(i :: fixednames) (a :: AExpr) ->
                case evalA [] a of
                  Left e -> vacous
                  Right a' -> runEval [] (Assign i (IntConst a')) === runEval [(i,a')] Skip
            , testProperty "Seq associative" $ \(s1 :: Stmt) (s2 :: Stmt) (s3 :: Stmt) ->
                runEval [] (Seq s1 (Seq s2 s3)) === runEval [] (Seq (Seq s1 s2) s3)
            , testProperty "Seq skip s"  $ \(s1 :: Stmt) ->
                runEval [] (Seq Skip s1) === runEval [] s1
            , testProperty "Seq Fail" $ \(s1 :: Stmt) ->
                 isLeft (runEval [] (Seq s1 Fail)) === isLeft (runEval [] (Seq Fail s1))
            , testProperty "If True" $ \(s1 :: Stmt) (s2 :: Stmt) ->
                runEval [] (If (BoolConst True) s1 s2) === runEval [] s1
            , testProperty "If False" $ \(s1 :: Stmt) (s2 :: Stmt) ->
                runEval [] (If (BoolConst False) s1 s2) === runEval [] s2
            , testProperty "If what??" $ \(s1 :: Stmt) ->
                runEval [] (If (BoolConst False) s1 s1) === runEval [] (If (BoolConst True) s1 s1)
            , testProperty "If swap" $ \(s1 :: Stmt) (s2 :: Stmt) (b :: BExpr) ->
                runEval [] (If b s1 s2) === runEval [] (If (Negate b) s2 s1)
            ]
        ]

vacous = True === True

evalA :: [(VName, Integer)] -> AExpr -> Err Integer
evalA xs ast =
  case evalRWST (evalAExpr ast) () (M.fromList xs) of
    Left e -> Left e
    Right (i, _) -> return i

evalB :: [(VName, Integer)] -> BExpr -> Err Bool
evalB xs ast =
  case evalRWST (evalBExpr ast) () (M.fromList xs) of
    Left e -> Left e
    Right (i, _) -> return i
