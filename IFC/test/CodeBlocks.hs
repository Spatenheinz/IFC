{-# OPTIONS_GHC -Wno-missing-signatures #-}
module CodeBlocks where

import           Test.Tasty.QuickCheck
import AST
import Control.Monad
import QCInstances
import Utils

whilenames = elements $ map (("__" ++) . (:[])) ['a'..'e']

whileConds = do
  let v = whilenames
      v' = Var <$> v
      ass = liftM2 Assign v arbitrary
      gt = liftM2 (RBinary Greater) v' arbitrary
      dec = liftM2 Assign v (liftM2 (ABinary Sub) v' (return $ IntConst 1))
      lt = liftM2 (RBinary Less) v' arbitrary
      inc = liftM2 Assign v (liftM2 (ABinary Add) v' (return $ IntConst 1))
      eq = liftM2 (RBinary Eq) v' arbitrary
      change = liftM2 Assign v arbitrary
  elements $ zip3 [ass, ass, ass] [gt, lt, eq] [dec, inc, change]
        -- neq = Negate BBinary Eq v' <$> arbitrary
        -- Food for thought


instance Arbitrary Stmt where
  arbitrary = sized stexpr
  shrink (Seq s1 s2) = [s1, s2] <> [Seq s1' s2' | (s1',s2') <- shrink (s1, s2)]
  shrink (GhostAss v a) = map (GhostAss v) $ shrink a
  shrink (Assign v a) = map (Assign v) $ shrink a
  shrink (If b s1 s2) = [s1, s2] <> [If b' s1' s2' | (b', s1',s2') <- shrink (b, s1,s2)]
  shrink (Asst f) = map Asst $ shrink f
  shrink (While b invs var s) = [s] <> [While b' invs' var' s |
                                        (b', invs', var', s')
                                        <- shrink (b, invs, var, s)]
  shrink Skip = [Skip]
  shrink Fail = [Fail]

stexpr 0 = stleaf
stexpr n = frequency [ (15, stleaf)
                   , (10, ifs $ subExpr n )
                   , (1, liftM2 Seq (seq1 (subExpr n)) (stexpr $ subExpr n))
                   -- , (1, while $ subExpr n)
                   ]
stleaf = frequency [ (1, return Skip)
                 , (1, return Fail)
                 , (2, assign)
                 , (1, ghost)
                 , (1, assert)
                 ]
seq1 n = frequency [(15, stleaf), (7, ifs n)] --, (1, while n)]
assign = do
  i <- fixednames
  Assign i <$> arbitrary
ghost = do
  i <- ghostid
  GhostAss i <$> arbitrary
ifs n = do
  b <- arbitrary
  e1 <- stexpr n
  e2 <- stexpr n
  return $ If b e1 e2
while n = do
  (bef, con, variant) <- whileConds
  bef' <- bef
  con' <- con
  variant' <- variant
  body <- stexpr n
  return $ Seq bef' (While con' (Cond (BoolConst True)) [] (Seq body variant'))
assert = Asst <$> arbitrary

instance Arbitrary FOL where
  arbitrary = sized fexpr
  shrink (Cond b) = map Cond $ shrink b
  shrink (Forall v f) = [Forall v' f' | (v', f') <- shrink (v, f) ]
  shrink (Exists v f) = [Exists v' f' | (v', f') <- shrink (v, f) ]
  shrink (AConj f1 f2) = [f1, f2] <> [aconj f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (ADisj f1 f2) = [f1, f2] <> [adisj f1' f2' | (f1',f2') <- shrink (f1,f2)]
  shrink (AImp f1 f2) = [f1, f2] <> [aimp f1' f2' | (f1',f2') <- shrink (f1,f2)]

fexpr 0 = fleaf
fexpr n = frequency [(5, fleaf)
                   , (1, onlM2 aconj (fexpr . subExpr) n n)
                   , (1, onlM2 adisj (fexpr . subExpr) n n)
                   , (1, onlM2 aimp (fexpr . subExpr) n n)
                   ]
fleaf = Cond <$> arbitrary
