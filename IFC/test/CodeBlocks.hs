module CodeBlocks where

import           Test.Tasty.QuickCheck
import AST
import Control.Monad
import QCInstances
import Data.Function (on)


whileConds = elements $ zip3 (replicate 4 ass) [gt, lt, eq] [dec, inc, change]
  where v = identifier
        v' = Var <$> v
        ass = liftM2 Assign v arbitrary
        gt = liftM2 (RBinary Greater) v' arbitrary
        dec = liftM2 Assign v (liftM2 (ABinary Sub) v' (return $ IntConst 1))
        lt = liftM2 (RBinary Less) v' arbitrary
        inc = liftM2 Assign v (liftM2 (ABinary Add) v' (return $ IntConst 1))
        eq = liftM2 (RBinary Eq) v' arbitrary
        change = liftM2 Assign v arbitrary
        -- neq = Negate BBinary Eq v' <$> arbitrary
        -- Food for thought


instance Arbitrary Stmt where
  arbitrary = sized expr
    where expr 0 = leaf
          expr n = frequency [ (5, leaf)
                             , (1, ifs $ subExpr n )
                             , (3, liftM2 Seq (expr $ subExpr n) (expr $ subExpr n))
                             , (1, while $ subExpr n)
                             ]
          leaf = frequency [ (2, return Skip)
                           , (1, return Fail)
                           , (5, assign)
                           -- , (1, ghost)
                           , (1, assert)
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
          while n = do
            (bef, con, variant) <- whileConds
            bef' <- bef
            con' <- con
            variant' <- variant
            body <- expr n
            return $ Seq bef' (While con' [] Nothing (Seq body variant'))
          assert = Asst <$> arbitrary

instance Arbitrary FOL where
  arbitrary = sized expr
    where expr 0 = leaf
          expr n = frequency [ (5, leaf)
                             , (1, on (liftM2 AConj) (expr . subExpr) n n)
                             , (1, on (liftM2 ADisj) (expr . subExpr) n n)
                             , (1, on (liftM2 AImp) (expr . subExpr) n n)
                             ]
          leaf = Cond <$> arbitrary
