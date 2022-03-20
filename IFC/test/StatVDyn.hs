{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StatVDyn where
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
import Data.SBV.Trans (ThmResult(ThmResult), Modelable (modelExists))
import Test.QuickCheck.Monadic (monadicIO, run)

dynstat = testGroup "Test between static and dynamic" [
        testGroup "QC" [
            testProperty "Eval ~ VC-SAT" $ \(s :: Stmt) ->
                case runEval [] s of
                  Left _ -> case proveWLP s ([],Nothing) of
                              Left _ -> True === True
                              Right p -> monadicIO $ do
                                res <- run $ prove p
                                return $ modelExists res
                  Right _ -> case proveWLP s ([],Nothing) of
                              Left _ -> False === True
                              Right p -> monadicIO $ run (prove p) >>= return . not . modelExists
        ]
        ]
