module CodeBlocks where

import           Test.Tasty.QuickCheck
import AST
import Control.Monad
import QCInstances

instance Arbitrary Stmt where
  arbitrary = sized expr
    where expr 0 = leaf
          expr n = frequency [ (5, leaf)
                             , (1, ifs $ subExpr n )
                             , (3, liftM2 Seq (expr $ subExpr n) (expr $ subExpr n))
                             ]
          leaf = frequency [ (2, return Skip)
                           , (1, return Fail)
                           , (5, assign)
                           , (1, ghost)
                           ]
          assign = do
            i <- fixednames
            Assign i <$> arbitrary
          ghost = do
            i <- ghostid
            GhostAss i <$> arbitrary
          ifs n = do
            b <- arbitrary
            e1 <- expr n
            e2 <- expr n
            return $ If b e1 e2
