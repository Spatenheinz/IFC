{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module QCInstances where

import           Test.Tasty.QuickCheck
import AST
import Control.Monad

subExpr = flip div 10

asciiLetter = elements $ ['a'..'z'] ++ ['A'..'Z']
hyphen      = elements ['_']
digitgen      = elements ['0'..'9']
identifier = do
  h <- oneof [asciiLetter,hyphen]
  size <- choose (0,3)
  t <- concat <$> replicateM 3 (vectorOf size (oneof [asciiLetter, hyphen, digitgen]))
  return $ (h:t)

fixednames = elements $ map (:[]) ['a' .. 'e']

ghostid = ("👻"++) <$> fixednames

instance Arbitrary ArithOp where
  arbitrary = elements [ Add .. Mod]

instance Arbitrary AExpr where
   arbitrary = sized aexpr
   shrink (IntConst i) = map IntConst $ shrink i
   shrink (Var v) = [Var v]
   shrink (Ghost v) = [Ghost v]
   shrink (Neg a) = map Neg $ shrink a
   shrink (ABinary op a1 a2) = [a1, a2] <> [ABinary op a1' a2' | (a1',a2') <- shrink (a1,a2)]

aexpr 0 = aleaf
aexpr n = frequency [ (5, aleaf)
                        , (2, operator $ subExpr n)
                        -- is --a allowed?
                        , (1, Neg <$> aexpr (subExpr n))
                        ]
aleaf = frequency [ (5, IntConst <$> arbitrary `suchThat` (> 0))
                        , (1, Var <$> fixednames)
                        -- , (1, Ghost <$> ghostid)
                        ]
operator n = do
  op <- arbitrary
  e1 <- aexpr n
  e2 <- aexpr n
  return $ abinary op e1 e2

instance Arbitrary BoolOp where
  arbitrary = elements [Conj, Disj]

instance Arbitrary ROp where
  arbitrary = elements [Less, Eq, Greater]



instance Arbitrary BExpr where
  arbitrary = sized bexpr
  shrink (BoolConst b) = map BoolConst $ shrink b
  shrink (Negate b) = map Negate $ shrink b
  shrink (BBinary op b1 b2) = [b1, b2] <> [BBinary op b1' b2' | (b1',b2') <- shrink (b1,b2)]
  shrink (RBinary op a1 a2) = [RBinary op a1' a2' | (a1',a2') <- shrink (a1,a2)]

bexpr 0 = bleaf
bexpr n = frequency [ (5, bleaf)
                        , (2, rop $ subExpr n)
                        -- , (5, bop $ subExpr n)
                        , (1, bnegate <$> bexpr (subExpr n))
                        ]
bleaf = frequency [ (1, BoolConst <$> arbitrary)
                        ]
bop n = do
        op <- arbitrary
        e1 <- bexpr n
        e2 <- bexpr n
        case op of
          Conj -> return $ bconj e1 e2
          Disj -> return $ bdisj e1 e2
rop n = do
        op <- arbitrary
        e1 <- arbitrary
        e2 <- arbitrary
        return $ RBinary op e1 e2

