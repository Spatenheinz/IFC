module Main where

import AST
import Eval
import Parser

import System.Exit (die)
import System.Environment (getArgs)

run :: Stmt -> IO ()
run p = case runEval [] p of
          Left e -> putStrLn ("*** Runtime error: \n") >> putStrLn e
          Right store -> printEval store

main :: IO ()
main = do args <- getArgs
          case args of
            -- ["-i", file] -> do
            --   s <- readFile file
            --   run $ read s
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> putStrLn $ show p
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> run p
            _ ->
              die "Usage:\n\
                    \  boa -i PROGRAM.ast    (interpret only)\n\
                    \  boa -p PROGRAM.boa    (parse only)\n\
                    \  boa PROGRAM.boa       (parse & interpret)"
