module AST where

import Data.SBV

data BExpr = BoolConst Bool
  | Negate BExpr
  | BBinary BoolOp BExpr BExpr
  | RBinary ROp AExpr AExpr
  deriving (Show)

data BoolOp = Conj | Disj
  deriving (Show)

data ROp = Less | Eq | Greater
  deriving (Show, Read, Eq)

data AExpr = Var VName
  | Ghost VName
  | IntConst Integer
  | Neg AExpr
  | ABinary ArithOp AExpr AExpr
  deriving (Show)

data ArithOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show)

data Stmt = Seq Stmt Stmt
  | GhostAss VName AExpr
  | Assign VName AExpr
  | If BExpr Stmt Stmt
  | Asst FOL
  | While BExpr [FOL] (Maybe Variant) Stmt
  | Skip
  | Fail
  deriving (Show)

type Variant = AExpr

data FOL = Cond BExpr
  | Forall VName FOL
  | Exists VName FOL
  | ANegate FOL
  | AConj FOL FOL
  | ADisj FOL FOL
  | AImp FOL FOL
  deriving (Show)

type VName = String

(./\.) :: FOL -> FOL -> FOL
(./\.) = AConj
infixr 3 ./\.
(.\/.) :: FOL -> FOL -> FOL
(.\/.) = ADisj
infixr 2 .\/.
(.=>.) :: FOL -> FOL -> FOL
(.=>.) = AImp
infixr 1 .=>.

-- To be able to substitute values in WLP we need an AST
-- as building it simply by SBV we get an SBool
