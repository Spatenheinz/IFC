{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StatVDyn where

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
import Data.SBV (prove, SMTResult(..))
import Data.SBV.Trans.Control
import WLP
import System.IO.Unsafe
import Data.SBV.Trans (ThmResult(ThmResult))

dynstat = testGroup "Test between static and dynamic" [
        testGroup "QC" [
            -- testProperty "Eval ~ VC-SAT" $ \(s :: Stmt) ->
            --     case runEval [] s of
            --       Left _ -> case proveWLP s ([],Nothing) of
            --                   Left _ -> True === True
            --                   Right p -> case unsafePerformIO $ prove p of
            --                               ThmResult (Satisfiable _ _) -> False === True
            --                               _ -> True === True
            --       Right _ -> case proveWLP s ([],Nothing) of
            --                   Left _ -> False === True
            --                   Right p -> case unsafePerformIO $ prove p of
            --                               ThmResult (Satisfiable _ _) -> True === True
            --                               _ -> False === True
        ]
        ]
