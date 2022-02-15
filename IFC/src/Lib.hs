{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PolyKinds                 #-}

module Lib where

import GHC.TypeLits

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- type family HasSymbol (g :: [(Symbol,*)]) (s :: Symbol) :: Maybe * where
--   HasSymbol '[]            s = 'Nothing
--   HasSymbol ('(s, a) ': g) s = 'Just a
--   HasSymbol ('(t, a) ': g) s = HasSymbol g s

-- -- Renamed Proxy types for clarity
-- data Name (s :: Symbol) = Var
-- data Type (a :: *)      = Of

-- example' :: Statement '[] ('("foo", Int) ': '[])
-- example' = Declare (Var :: Name "foo") (Of :: Type Int)

-- data ScopedSymbol (g :: [(Symbol,*)]) (a :: *) = forall s.
--   (HasSymbol g s ~ 'Just a) => The (Name s)

-- instance Show (ScopedSymbol g a) where
--   show (The s) = show s

-- example :: ScopedSymbol ('("foo", Int) ': '("bar", Bool) ': '[]) Bool
-- example = The (Var :: Name "bar")

-- example'' :: Statement ('("foo", Int) ': '[]) ('("foo", Int) ': '[])
-- example'' = Assign (The (Var :: Name "foo")) (EInt 1)

-- data Statement (g :: [(Symbol, *)]) (h :: [(Symbol,*)]) where
--   Declare :: Name s -> Type a -> Statement g ('(s, a) ': g)
--   Assign  :: ScopedSymbol g a -> Exp g a -> Statement g g

-- infixr 5 :>
-- data Statements (g :: [(Symbol, *)]) (h :: [(Symbol,*)]) where
--   Done :: Statements g g
--   (:>) :: Statement g h -> Statements h i -> Statements g i

-- data Program = forall h. Program (Statements '[] h)

-- increment :: ScopedSymbol g Int -> Statement g g
-- increment v = Assign v (EAdd (EVar v) (EInt 1))

-- program :: Program
-- program = Program
--   $  Declare (Var :: Name "foo") (Of :: Type Int)
--   :> Assign  (The (Var :: Name "foo")) (EInt 1)
--   :> Declare (Var :: Name "bar") (Of :: Type Bool)
--   :> increment (The (Var :: Name "foo"))
--   :> Assign  (The (Var :: Name "bar")) (ENot $ EBool True)
--   :> Done

-- data Exp (g :: [(Symbol,*)]) (t :: *) where
--   EVar    :: ScopedSymbol g a -> Exp g a
--   EBool   :: Bool -> Exp g Bool
--   EInt    :: Int  -> Exp g Int
--   EAdd    :: Exp g Int -> Exp g Int -> Exp g Int
--   ENot    :: Exp g Bool -> Exp g Bool

-- instance Show (Exp g t) where
--   show (EVar _) = "variable"
--   show (EBool b) = show b
--   show (EInt i) = show i
--   show (EAdd e1 e2) = show e1 ++ " " ++ show e2
--   show (ENot e1) = "-" ++ show e1
