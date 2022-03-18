module QCInstances where

import           Test.Tasty.QuickCheck
import AST
import Control.Monad

subExpr = flip div 2

asciiLetter = elements $ ['a'..'z'] ++ ['A'..'Z']
hyphen      = elements ['_']
digitgen      = elements ['0'..'9']
identifier = do
  h <- oneof [asciiLetter,hyphen]
  size <- choose (0,3)
  t <- concat <$> replicateM 3 (vectorOf size (oneof [asciiLetter, hyphen, digitgen]))
  return $ (h:t)

fixednames = elements $ map show ['a' .. 'z']

ghostid = ('👻':) <$> fixednames

instance Arbitrary ArithOp where
  arbitrary = elements [ Add .. Mod]

instance Arbitrary AExpr where
   arbitrary = sized expr
      where
        expr 0 = leaf
        expr n = frequency [ (5, leaf)
                           , (5, operator $ subExpr n)
                           , (1, Neg <$> (expr $ subExpr n))
                           ]
        leaf = frequency [ (1, IntConst <$> arbitrary)
                         , (1, Var <$> fixednames)
                         , (1, Ghost <$> ghostid)
                         ]
        operator n = do
          op <- arbitrary
          e1 <- expr n
          e2 <- expr n
          return $ abinary op e1 e2

instance Arbitrary BoolOp where
  arbitrary = elements [Conj, Disj]
instance Arbitrary ROp where
  arbitrary = elements [Less, Eq, Greater]

instance Arbitrary BExpr where
  arbitrary = sized expr
      where
        expr 0 = leaf
        expr n = frequency [ (5, leaf)
                           , (5, rop $ subExpr n)
                           , (5, bop $ subExpr n)
                           , (1, bnegate <$> (expr $ subExpr n))
                           ]
        leaf = frequency [ (1, BoolConst <$> arbitrary)
                         ]
        bop n = do
          op <- arbitrary
          e1 <- expr n
          e2 <- expr n
          case op of
            Conj -> return $ bconj e1 e2
            Disj -> return $ bdisj e1 e2
        rop n = do
          op <- arbitrary
          e1 <- arbitrary
          e2 <- arbitrary
          return $ RBinary op e1 e2
        subExpr = flip div 2
