module Main where

import AST
import Eval
import Parser
import WLP
import System.Exit (die)
import System.Environment (getArgs)
import Data.SBV (prove)
import Data.SBV.Trans (ThmResult)
import Control.Monad (void)
import Pretty

run :: Stmt -> IO ()
run p = case runEval [] p of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right store -> printEval store

formular :: Stmt -> IO ()
formular p = case runWLP [] p of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right (f,s) -> putStrLn (prettyF f 4) >> print s

prover :: Stmt -> IO ThmResult
prover p = case proveWLP [] p of
             Left e -> error "hmm ok then"
             Right f -> prove f

main :: IO ()
main = do args <- getArgs
          case args of
            ["-q", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> prover p >>= print
            ["-f", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> formular p
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> print p
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> run p
            _ ->
              die "Usage:\n\
                    \  IFC -f PROGRAM.ast    (interpret only)\n\
                    \  IFC -p PROGRAM.boa    (parse only)\n\
                    \  boa PROGRAM.boa       (parse & interpret)"
