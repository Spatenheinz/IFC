{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import AST
import Eval
import Parser
import WLP
import qualified WLP2 as W
import System.Exit (die)
import System.Environment (getArgs)
import Data.SBV.Trans.Control
import Control.Monad (void)
import qualified Data.Map as M
import Pretty
import OptParser
import Control.Monad.Except
import Data.SBV.Trans
import Debug.Trace
-- import qualified WLP2 as W

run :: Stmt -> PreConds -> [(VName, Integer)] -> IO ()
run p (_,pre) st = case maybe (runEval st p) (\x -> runEval st (Seq (Asst x) p)) pre of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right store -> printEval store

formular :: Stmt -> ([VName], Maybe FOL) -> IO ()
formular p st = case runWLP p st of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right (f,s) -> putStrLn (prettyF f 4 <> "\n\n") >> print s

prover2 :: Stmt -> IO ThmResult
prover2 p = case W.proveWLP [] p of
             Left e -> error "hmm ok then"
             Right f -> prove f

formular1 :: Stmt -> IO ()
formular1 p = case W.runWLP [] p of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right f -> print  f

prover :: Stmt -> ([VName], Maybe FOL) -> IO ThmResult
prover p st = case proveWLP p st of
             Left e -> error e
             Right f -> do
               p' <- runExceptT $ prove f
               case p' of
                 Left e -> error e
                 Right p'' -> return p''

main :: IO ()
main = do args <- getArgs
          case args of
            ["-q2", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> prover2 p >>= print
            ["-q", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> prover p st >>= print
            ["-f2", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> formular1 p
            ["-f", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> formular p st
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> print p
            [file, argslist] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right (p,st) -> case parseStore argslist of
                            Left e -> print e
                            Right args -> run p st args
            _ ->
              die "Usage:\n\
                    \  IFC FIX       (parse & interpret)"

-- testOfMul :: IO ThmResult
-- testOfMul = prove $ fToS thing M.empty
--   where thing =
--          Forall "q#3" ((Cond (RBinary Eq (Var "q#3") (IntConst 10))) .=>.
--           Forall "r#1" ((Cond (RBinary Eq (Var "r#1") (IntConst 55))) .=>.
--                 (ANegate (Cond (RBinary Less (Var "q#3") (IntConst 0))))
--                 ./\. (ANegate (Cond (RBinary Less (Var "r#1") (IntConst 0))))
--                 ./\. Forall "res#3" ((Cond (RBinary Eq (Var "res#3") (IntConst 0))) .=>.
--                 Forall "ghosta" ((Cond (RBinary Eq (Var "ghosta") (Var "q#3"))) .=>.
--                 (Cond (RBinary Eq (Var "res#3") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#3")) (Var "r#1"))))
--                 ./\. Forall "q#1" (Forall "res#1" (((Cond (RBinary Greater (Var "q#1") (IntConst 0))) ./\. (Cond (RBinary Eq (Var "res#1") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#1")) (Var "r#1")))))
--                         .=>. (Forall "res#2" ((Cond (RBinary Eq (Var "res2") (ABinary Add (Var "res#1") (Var "r#1"))))
--                         .=>. Forall "q#2" ((Cond (RBinary Eq (Var "q#2") (ABinary Sub (Var "q#1") (IntConst 1))))
--                         .=>. (Cond (RBinary Eq (Var "res#2") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#2")) (Var "r#1"))))))
--                         ./\. ((ANegate (Cond (RBinary Greater (Var "q#1") (IntConst 0)))) ./\. (Cond (RBinary Eq (Var "res#1") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#1")) (Var "r#1"))))
--                         .=>. (Cond (RBinary Eq (Var "res#1") (ABinary Mul (Var "ghosta") (Var "r#1"))))))))))))
