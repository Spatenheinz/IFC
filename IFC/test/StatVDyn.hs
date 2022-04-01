{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StatVDyn where
import AST

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Eval
import qualified Data.Map as M
import Data.SBV.Trans as T hiding ((===))
import Test.QuickCheck.Monadic (monadicIO, run)
import Control.Monad.Except
import Parser
import ProveAPI
import System.IO.Unsafe
import Data.SBV.Internals
import Data.Maybe
import Data.List

dynstat :: TestTree
dynstat = testGroup "Test between static and dynamic" [
        -- testGroup "QC" [
        --     testProperty "Eval ~ VC-SAT" $ \(s :: Stmt) ->
        --         cover 80 (isRight $ runEval [] s) "non-trivial" $
        --         case runEval [] s of
        --           Left _ -> qcProver s (property True) id
        --           Right _ -> qcProver s (property False) not
        --     , testProperty "Eval true ~ VC-SAT" $ \(s :: Stmt) ->
        --         isRight (runEval [] s) ==> qcProver s (property False) not
        --     , testProperty "Eval true ~ VC-SAT" $ \(s :: Stmt) ->
        --         unsafePerformIO (qcProver2 s not) ==> isRight (runEval [] s)
        --     ],
        testGroup "random input" [
            -- always_wrong
            testProperty "assign" $ \(i :: Integer) -> runProg "assign" [("i", i)] ("n", i+10),
            -- collatz
            testProperty "div" $ \(i1 :: NonNegative Integer) (i2 :: Positive Integer) ->
                let i1' = getNonNegative i1
                    i2' = getPositive i2
                in  runProg "div" [("a", i1'),("b",i2')] ("q", i1' `div` i2'),
            testProperty "factorial" $ \(i1 :: Positive Int) ->
                let i1' = getPositive i1
                in runProg "fac" [("r", toInteger i1')] ("q", fact !! i1' ),
            -- fakesum
            testProperty "fibonacci" $ \(i1 :: Positive Int) ->
                let i1' = getPositive i1
                in runProg "fib" [("a", toInteger i1')] ("res", fibs !! i1'),
            testProperty "isqrt" $ \(i1 :: NonNegative Integer) ->
                let i1' = getNonNegative i1
                in runProg "isqrt" [("x", i1')] ("res", isqrt i1'),
            localOption (mkTimeout 4000000) $ testProperty "isqrt_fast" $ \(i1 :: NonNegative Integer) ->
                let i1' = getNonNegative i1
                in runProg "isqrt_fast" [("x", i1')] ("res", isqrt i1'),
            testProperty "isqrt_sub" $ \(i1 :: NonNegative Integer) ->
                let i1' = getNonNegative i1
                in runProg "isqrt_sub" [("x", i1')] ("res", isqrt i1'),
            testProperty "mccarthy" $ \(i :: Integer) ->
                runProg "mccarthy" [("x", i)] ("n", mc91 i),
            -- mod0
            testProperty "mult" $ \(i1 :: NonNegative Integer) (i2 :: Integer) ->
                let i1' = getNonNegative i1
                in runProg "mult" [("q", i1'), ("r", i2)] ("res", i1' * i2),
            localOption (mkTimeout 4000000) $ testProperty "mult3" $ \(i1 :: NonNegative Integer) (i2 :: NonNegative Integer) (i3 :: Integer) ->
                let i1' = getNonNegative i1
                    i2' = getNonNegative i2
                in runProg "mult3" [("q", i1'), ("r", i2'), ("s", i3)] ("res", i1' * i2' * i3),
            testProperty "max" $ \(i1 :: Integer) (i2 :: Integer) ->
                runProg "max" [("x", i1), ("y", i2)] ("x", max i1 i2),
            testProperty "simple" $ \(i1 :: Positive Integer) ->
                let i1' = getPositive i1
                in even i1' ==> runProg "simple" [("y", i1'), ("q", 1)] ("y", 0),
            testProperty "skip" $ \(i1 :: Integer) ->
                runProg "skip" [("a", i1)] ("a", i1),
            testProperty "sum" $ \(i1 :: Positive Integer) ->
                let i1' = getPositive i1
                in runProg "sum" [("n", i1')] ("sum", sum [0..i1'])
            -- undefined
            ],
        testGroup "Runs and Proves" [
            testCase "*always_wrong" $ imply (proofResult "always_wrong") "always_wrong" [] ("res", 1),
            testCase "assign" $ imply (proofResult "assign") "assign" [("i", 42)] ("n", 42+10),
            testCase "collatz" $ imply (proofResult "collatz") "collatz" [("n", 42)] ("k", 42),
            testCase "div" $ imply (proofResult "div")  "div" [("a", 42),("b",10)] ("q", 42 `div` 10),
            testCase "factorial" $ imply (proofResult "fac") "fac" [("r", 42)] ("q", fact !! 42 ),
            testCase "fibonacci" $ imply (proofResult "fib") "fib" [("a", 42)] ("res", fibs !! 42),
            testCase "isqrt" $ imply (proofResult "isqrt") "isqrt" [("x", 42)] ("res", isqrt 42),
            localOption (mkTimeout 4000000) $ testCase "isqrt fast" $ imply (proofResult "isqrt_fast") "isqrt_fast" [("x", 42)] ("res", isqrt 42),
            testCase "isqrt sub" $ imply (proofResult "isqrt_sub") "isqrt_sub" [("x", 42)] ("res", isqrt 42),
            testCase "max" $ imply (proofResult "max")  "max" [("x", 42), ("y", 420)] ("x", max 42 420),
            testCase "mult" $ imply (proofResult "mult") "mult" [("q", 10), ("r", 42)] ("res", 10 * 42),
            testCase "mult3" $ imply (proofResult "mult3") "mult3" [("q", 10), ("r", 42),("s",420)] ("res", 10 * 42 * 420),
            testCase "simple" $ imply (proofResult "simple") "simple" [("y", 10), ("q", 1)] ("y", 0),
            testCase "skip" $ imply (proofResult "skip") "skip" [("a", 42)] ("a", 42),
            testCase "sum" $ imply (proofResult "sum") "sum" [("n", 420)] ("sum", sum [0..420])
            ],
        testGroup "Fails (abnormally)" [
            testCase "*div_in_cond" $ fails "div_in_cond" [("x", 0)] "Division",
            testCase "*fakesum" $ fails "fakesum" [("n", 42)] "Assertion",
            testCase "*mod0" $ fails "mod0" [] "Modulo",
            testCase "*undefined" $ fails "undef" [] "Modulo"
                                       ],
        testGroup "Unexpressive" [
            testCase "*false_mult" $ notSame (fst $ proofResult "false_mult") "false_mult" [("q", 42),("r",10)] ("res", 420),
            testCase "*mccarthy" $ notSame (fst $ proofResult "mccarthy") "mccarthy" [("x", 42)] ("n", 91)
                                 ]
        ]

notSame :: Bool -> String -> [(VName, Integer)] -> (VName, Integer) -> Assertion
notSame res prog st (i,target) = do
  p <- readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left _ -> assertBool "read file wrong" False
    Right (p', (_,f)) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) f of
                        Left e -> assertBool e False
                        Right store -> case M.lookup i store of
                                        Nothing -> assertBool ("Var " <> i <> " not found") False
                                        Just a -> assertBool (show a <> "does not match target: " <> show target) $ a == target && not res

isqrt :: Integer -> Integer
isqrt = fromIntegral . floor . sqrt . fromInteger
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 $ mc91 $ n + 11
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fact :: [Integer]
fact = 1 : zipWith (*) fact [1..]

imply :: (Bool, M.Map String CV) -> String -> [(VName, Integer)] -> (VName, Integer) -> Assertion
imply (a, m) prog st (i, target) = if a then runProg2 prog st (i,target)
  else
  let st' = foldr (\(x,_) acc -> (x, cvToInteger $ fromJust $ M.lookup x m):acc) [] st-- fold
  in failProg prog st'

cvToInteger :: CV -> Integer
cvToInteger c = case cvVal c of
                  CInteger i -> i
                  _ -> error "not an integer"


fails :: String -> [(VName, Integer)] -> String -> Assertion
fails prog st target = do
  p <- readFile $ "examples/" <> prog <> ".ifc"
  let (pr, _m) = proofResult prog
  case parseString p of
    Left _ -> assertBool "read file wrong" False
    Right (p', (_,f)) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) f of
                        Left e -> assertBool (e <> show pr) $ target `isPrefixOf` e && not pr
                        Right _ -> assertBool "should not run correctly" True


failProg :: String -> [(VName, Integer)] -> Assertion
failProg prog st = do
  p <- readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left e -> error e
    Right (p', (_,st')) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) st' of
          Left e -> assertBool e True
          Right _ -> assertBool "Program did not fail but was expected to" False

runProg :: String -> [(VName, Integer)] -> (VName, Integer) -> Property
runProg prog st (i, target) = monadicIO $ do
  p <- run $ readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left e -> error e
    Right (p', (_,st')) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) st' of
          Left _ -> return $ property False
          Right store -> case M.lookup i store of
                           Nothing -> return $ property False
                           Just a -> return $ a === target

runProg2 :: String -> [(VName, Integer)] -> (VName, Integer) -> Assertion
runProg2 prog st (i, target) = do
  p <- readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left e -> error e
    Right (p', (_,st')) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) st' of
          Left e -> assertBool e False
          Right store -> case M.lookup i store of
                           Nothing -> assertBool ("cant find var " <> i) False
                           Just a -> assertBool "did not do what we expected" $ a == target

proofResult :: String -> (Bool, M.Map String CV)
proofResult prog = unsafePerformIO $ do
  p <- readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left e -> error e
    Right (ast,st) -> case proveWLP ast st of
                   Left _ -> return (False, M.empty)
                   Right f -> do
                        res <- runExceptT (T.prove f :: ExceptT String IO ThmResult)
                        case res of
                          Left e' -> error e'
                          Right r -> return (not $ modelExists r, getModelDictionary r)

-- qcProver :: Stmt -> Property -> (Bool -> Bool) -> Property
-- qcProver p prop mneg = case proveWLP p ([],Nothing) of
--                     Left e -> prop
--                     Right f -> monadicIO $ do
--                        res <- run $ runExceptT (T.prove f :: ExceptT String IO ThmResult)
--                        case res of
--                          Left e -> return prop
--                          Right r -> return . property $ mneg $ modelExists r

-- qcProver2 :: Stmt -> (Bool -> Bool) -> IO Bool
-- qcProver2 p mneg = case proveWLP p ([],Nothing) of
--                     Left e -> return False
--                     Right f -> do
--                        res <- runExceptT (T.prove f :: ExceptT String IO ThmResult)
--                        case res of
--                          Left e -> return False
--                          Right r -> return $ mneg $ modelExists r
