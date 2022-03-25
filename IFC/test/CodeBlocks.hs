{-# LANGUAGE BangPatterns #-}
module CodeBlocks where

import           Test.Tasty.QuickCheck
import AST
import Control.Monad
import QCInstances
import Data.Function (on)
import Test.QuickCheck.Gen (variant)
import Utils

whilenames = elements $ map (("__" ++) . (:[])) ['a'..'e']

whileConds = do
  let !x = whilenames
  let !v = x
      !v' = Var <$> v
      !ass = liftM2 Assign v arbitrary
      !gt = liftM2 (RBinary Greater) v' arbitrary
      !dec = liftM2 Assign v (liftM2 (ABinary Sub) v' (return $ IntConst 1))
      !lt = liftM2 (RBinary Less) v' arbitrary
      !inc = liftM2 Assign v (liftM2 (ABinary Add) v' (return $ IntConst 1))
      !eq = liftM2 (RBinary Eq) v' arbitrary
      !change = liftM2 Assign v arbitrary
  elements $ zip3 [ass, ass, ass] [gt, lt, eq] [dec, inc, change]
        -- neq = Negate BBinary Eq v' <$> arbitrary
        -- Food for thought


instance Arbitrary Stmt where
  arbitrary = sized expr
    where expr 0 = leaf
          expr n = frequency [ (5, leaf)
                             , (1, ifs $ subExpr n )
                             , (3, onlM2 Seq (expr . subExpr) n n)
                             , (1, while $ subExpr n)
                             ]
          leaf = frequency [ (2, return Skip)
                           , (1, return Fail)
                           , (5, assign)
                           -- , (1, ghost)
                           , (2, assert)
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
            return $ Seq bef' (While con' [Cond (BoolConst True)] Nothing (Seq body variant'))
          assert = Asst <$> arbitrary
  shrink (Seq s1 s2) = [s1, s2] <> [Seq s1' s2' | (s1',s2') <- shrink (s1,s2)]
  shrink (GhostAss v a) = map (GhostAss v) $ shrink a
  shrink (Assign v a) = map (GhostAss v) $ shrink a
  shrink (If b s1 s2) = [s1, s2] <> [If b' s1' s2' | (b', s1',s2') <- shrink (b, s1,s2)]
  shrink (Asst f) = map Asst $ shrink f
  shrink (While b invs var s) = [s] <> [While b' invs' var' s | (b', invs', var', s') <- shrink (b, invs, var, s)]
  shrink Skip = [Skip]
  shrink Fail = [Fail]


instance Arbitrary FOL where
  arbitrary = sized expr
    where expr 0 = leaf
          expr n = frequency [ (5, leaf)
                             , (1, onlM2 AConj (expr . subExpr) n n)
                             , (1, onlM2 ADisj (expr . subExpr) n n)
                             , (1, onlM2 AImp (expr . subExpr) n n)
                             ]
          leaf = Cond <$> arbitrary -- This should not be able to do BBinary
  shrink (Cond b) = map Cond $ shrink b
  shrink (Forall v f) = [Forall v' f' | (v', f') <- shrink (v, f) ]
  shrink (Exists v f) = [Forall v' f' | (v', f') <- shrink (v, f) ]
  shrink (AConj f1 f2) = [f1, f2] <> [AConj f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (ADisj f1 f2) = [f1, f2] <> [ADisj f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (AImp f1 f2) = [f1, f2] <> [AImp f1' f2' | (f1',f2') <- shrink (f1,f2)]
