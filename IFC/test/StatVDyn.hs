{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module StatVDyn where
import AST
import QCInstances
import CodeBlocks

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===), arbitrary, Property, property)
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

dynstat = testGroup "Test between static and dynamic" [
        testGroup "QC" [
            testProperty "Eval ~ VC-SAT" $ \(s :: Stmt) ->
                case runEval [] s of
                  Left _ -> qcProver s (property True) id
                  Right _ -> qcProver s (property False) not
        ]
        ]

qcProver :: Stmt -> Property -> (Bool -> Bool) -> Property
qcProver p prop mneg = case proveWLP p ([],Nothing) of
                    Left e -> prop
                    Right f -> monadicIO $ do
                       res <- run $ runExceptT $ (T.prove f :: ExceptT String IO ThmResult)
                       case res of
                         Left e -> return prop
                         Right r -> return . property $ mneg $ modelExists r
