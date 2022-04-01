{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import AST
import Eval
import Parser
import WP
import ProveAPI
import System.Exit (die)
import System.Environment (getArgs)
import Pretty
import OptParser
import Control.Monad.Except
import Data.SBV.Trans

run :: Stmt -> Header -> [(VName, Integer)] -> IO ()
run p (_,pre) st = case maybe (runEval st p) (\x -> runEval st (Seq (Asst x) p)) pre of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right store -> printEval store

formular :: Stmt -> ([VName], Maybe FOL) -> IO ()
formular p st = case runWLP p st of
          Left e -> putStrLn "*** Runtime error:" >> putStrLn e
          Right (f,s) -> putStrLn (prettyF f 4 <> "\n\n") >> print s

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
            ["-v", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> prover p st >>= print
            ["-vc", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> formular p st
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,st) -> putStrLn $ prettyProgram p st
            ["-ast", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn "*** Parse error: \n" >> putStrLn e
                Right (p,_) -> print p --putStrLn $ prettyProgram p st
            ("-e":file:argslist) -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right (p,st) -> case parseStore (concat argslist) of
                            Left e -> die e
                            Right a -> run p st a
            _ ->
              die "Usage:\n\
                    \  IFC -e   program.ifc st  (parse & interpret with initial store st :: [(var,value)])\n\
                    \  IFC -p   program.ifc     (prettyprint program)\n\
                    \  IFC -ast program.ifc     (parses outputs AST)\n\
                    \  IFC -vc  program.ifc     (prints verification condition)\n\
                    \  IFC -v   program.ifc     (run verification condition through Z3)"
