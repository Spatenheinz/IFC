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

fixednames = elements $ map (:[]) ['a' .. 'e']

ghostid = ('👻':) <$> fixednames

instance Arbitrary ArithOp where
  arbitrary = elements [ Add .. Mod]

instance Arbitrary AExpr where
   arbitrary = sized expr
      where
        expr 0 = leaf
        expr n = frequency [ (5, leaf)
                           , (3, operator $ subExpr n)
                           -- is --a allowed?
                           , (1, Neg <$> expr (subExpr n))
                           ]
        leaf = frequency [ (100, IntConst <$> arbitrary `suchThat` (> 0))
                         , (30, Var <$> fixednames)
                         -- , (1, Ghost <$> ghostid)
                         ]
        operator n = do
          op <- arbitrary
          e1 <- expr n
          e2 <- expr n
          return $ abinary op e1 e2
   shrink (IntConst i) = map IntConst $ shrink i
   shrink (Var v) = [Var v]
   shrink (Ghost v) = [Ghost v]
   shrink (Neg a) = map Neg $ shrink a
   shrink (ABinary op a1 a2) = [a1, a2] <> [ABinary op a1' a2' | (a1',a2') <- shrink (a1,a2)]


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
                           , (1, bnegate <$> expr (subExpr n))
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
  shrink (BoolConst b) = map BoolConst $ shrink b
  shrink (Negate b) = map Negate $ shrink b
  shrink (BBinary op b1 b2) = [b1, b2] <> [BBinary op b1' b2' | (b1',b2') <- shrink (b1,b2)]
  shrink (RBinary op a1 a2) = [RBinary op a1' a2' | (a1',a2') <- shrink (a1,a2)]
