{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StatVDyn where
import AST
import QCInstances
import CodeBlocks

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===), (==>), arbitrary, Property, property, cover)
import Eval
import qualified Data.Map as M
import Data.Either
import Control.Monad
import Control.Monad.RWS
import Data.SBV.Trans as T hiding ((===))
import Data.SBV.Trans.Control
import WLP
import System.IO.Unsafe
import Test.QuickCheck.Monadic (monadicIO, run)
import Control.Monad.Except
import Parser

dynstat = testGroup "Test between static and dynamic" [
        testGroup "QC" [
            testProperty "Eval ~ VC-SAT" $ \(s :: Stmt) ->
                cover 80 (isRight $ runEval [] s) "non-trivial" $
                case runEval [] s of
                  Left _ -> qcProver s (property True) id
                  Right _ -> qcProver s (property False) not
            , testProperty "Eval true ~ VC-SAT" $ \(s :: Stmt) ->
                isRight (runEval [] s) ==> qcProver s (property False) not
            , testProperty "Eval true ~ VC-SAT" $ \(s :: Stmt) ->
                unsafePerformIO (qcProver2 s not) ==> isRight (runEval [] s)
            ],
        testGroup "random input" [
            testProperty "mult" $ \(i1 :: Integer) (i2 :: Integer) ->
                i1 >= 0 && i2 >= 0 ==> runProg "mult" [("q", i1), ("r", i2)] ("res", i1 * i2),
            testProperty "assign" $ \(i :: Integer) -> runProg "assign" [("i", i)] ("n", i+10),
            testProperty "euclidean div" $ \(i1 :: Integer) (i2 :: Integer) ->
                i1 >= 0 && i2 > 0 ==> runProg "div" [("a", i1),("b",i2)] ("q", i1 `div` i2),
            testProperty "factorial" $ \(i1 :: Int) ->
                i1 > 0 ==> runProg "fac" [("r", toInteger i1)] ("q", fact !! i1 ),
            testProperty "fibonacci" $ \(i1 :: Int) ->
                i1 > 0 ==> runProg "fib" [("a", toInteger i1)] ("res", fibs !! i1),
            testProperty "max" $ \(i1 :: Integer) (i2 :: Integer) ->
                runProg "max" [("x", i1), ("y", i2)] ("x", max i1 i2),
            testProperty "skip" $ \(i1 :: Integer) ->
                runProg "skip" [("a", i1)] ("a", i1),
            testProperty "sum" $ \(i1 :: Integer) ->
                i1 > 0 ==> runProg "sum" [("n", i1)] ("sum", sum [0..i1])
            ]
        ]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fact = 1 : zipWith (*) fact [1..]

runProg :: String -> [(VName, Integer)] -> (VName, Integer) -> Property
runProg prog st (i, target) = monadicIO $ do
  p <- run $ readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left _ -> return $ property False
    Right (p', (_,st')) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) st' of
          Left _ -> return $ property False
          Right store -> case M.lookup i store of
                           Nothing -> return $ property False
                           Just a -> return $ a === target

qcProver :: Stmt -> Property -> (Bool -> Bool) -> Property
qcProver p prop mneg = case proveWLP p ([],Nothing) of
                    Left e -> prop
                    Right f -> monadicIO $ do
                       res <- run $ runExceptT (T.prove f :: ExceptT String IO ThmResult)
                       case res of
                         Left e -> return prop
                         Right r -> return . property $ mneg $ modelExists r

qcProver2 :: Stmt -> (Bool -> Bool) -> IO Bool
qcProver2 p mneg = case proveWLP p ([],Nothing) of
                    Left e -> return False
                    Right f -> do
                       res <- runExceptT (T.prove f :: ExceptT String IO ThmResult)
                       case res of
                         Left e -> return False
                         Right r -> return $ mneg $ modelExists r
