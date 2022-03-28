{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StatVDyn where
import AST

import Test.Tasty
import Test.Tasty.QuickCheck
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
            testProperty "mult" $ \(i1 :: NonNegative Integer) (i2 :: NonNegative Integer) ->
                let i1' = getNonNegative i1
                    i2' = getNonNegative i2
                in runProg "mult" [("q", i1'), ("r", i2')] ("res", i1' * i2'),
            testProperty "assign" $ \(i :: Integer) -> runProg "assign" [("i", i)] ("n", i+10),
            testProperty "euclidean div" $ \(i1 :: NonNegative Integer) (i2 :: Positive Integer) ->
                let i1' = getNonNegative i1
                    i2' = getPositive i2
                in  runProg "div" [("a", i1'),("b",i2')] ("q", i1' `div` i2'),
            testProperty "factorial" $ \(i1 :: Positive Int) ->
                let i1' = getPositive i1
                in runProg "fac" [("r", toInteger i1')] ("q", fact !! i1' ),
            testProperty "fibonacci" $ \(i1 :: Positive Int) ->
                let i1' = getPositive i1
                in runProg "fib" [("a", toInteger i1')] ("res", fibs !! i1'),
            testProperty "max" $ \(i1 :: Integer) (i2 :: Integer) ->
                runProg "max" [("x", i1), ("y", i2)] ("x", max i1 i2),
            testProperty "skip" $ \(i1 :: Integer) ->
                runProg "skip" [("a", i1)] ("a", i1),
            testProperty "sum" $ \(i1 :: Positive Integer) ->
                let i1' = getPositive i1
                in runProg "sum" [("n", i1')] ("sum", sum [0..i1'])
            ],
        testGroup "Runs and Proves" [
            testProperty "mult" $ \(i1 :: NonNegative Integer) (i2 :: NonNegative Integer) ->
                let i1' = getNonNegative i1
                    i2' = getNonNegative i2
                in imply (proofResult "mult") "mult" [("q", i1'), ("r", i2')] ("res", i1' * i2'),
            testProperty "assign" $ \(i :: Integer) ->
                imply (proofResult "assign") "assign" [("i", i)] ("n", i+10),
            testProperty "euclidean div" $ \(i1 :: NonNegative Integer) (i2 :: Positive Integer) ->
                let i1' = getNonNegative i1
                    i2' = getPositive i2
                in  imply (proofResult "div")  "div" [("a", i1'),("b",i2')] ("q", i1' `div` i2'),
            testProperty "factorial" $ \(i1 :: Positive Int) ->
                let i1' = getPositive i1
                in imply (proofResult "fac") "fac" [("r", toInteger i1')] ("q", fact !! i1' ),
            testProperty "fibonacci" $ \(i1 :: Positive Int) ->
                let i1' = getPositive i1
                in imply (proofResult "fib") "fib" [("a", toInteger i1')] ("res", fibs !! i1'),
            testProperty "max" $ \(i1 :: Integer) (i2 :: Integer) ->
                imply (proofResult "max")  "max" [("x", i1), ("y", i2)] ("x", max i1 i2),
            testProperty "skip" $ \(i1 :: Integer) ->
                imply (proofResult "skip") "skip" [("a", i1)] ("a", i1),
            testProperty "sum" $ \(i1 :: Positive Integer) ->
                let i1' = getPositive i1
                in imply (proofResult "sum") "sum" [("n", i1')] ("sum", sum [0..i1'])
           ]
        ]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fact = 1 : zipWith (*) fact [1..]

imply :: (Bool, M.Map String CV) -> String -> [(VName, Integer)] -> (VName, Integer) -> Property
imply (a, m) prog st (i, target) = if a then runProg prog st (i,target)
  else
  let st' = foldr (\(x,_) acc -> (x, cvToInteger $ fromJust $ M.lookup x m):acc) [] st-- fold
  in failProg prog st'

cvToInteger :: CV -> Integer
cvToInteger c = case cvVal c of
                  CInteger i -> i
                  _ -> error "not an integer"

-- testRPOK :: String -> String -> TestTree
-- testRPOK desc prog prop =
--   testCase desc $ assert ("Parsing: " ++ con) (Right (s,([],Nothing)))
--     (proofResult prog)

failProg :: String -> [(VName, Integer)] -> Property
failProg prog st = monadicIO $ do
  p <- run $ readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left _ -> return $ property False
    Right (p', (_,st')) -> case maybe (runEval st p') (\x -> runEval st (Seq (Asst x) p')) st' of
          Left _ -> return $ property True
          Right _ -> return $ property False

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

proofResult :: String -> (Bool, M.Map String CV)
proofResult prog = unsafePerformIO $ do
  p <- readFile $ "examples/" <> prog <> ".ifc"
  case parseString p of
    Left _ -> return (False, M.empty)
    Right (ast,st) -> case proveWLP ast st of
                   Left _ -> return (False, M.empty)
                   Right f -> do
                        res <- runExceptT (T.prove f :: ExceptT String IO ThmResult)
                        case res of
                          Left _ -> error "bad program"
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
