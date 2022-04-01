module AST where

type Header = ([VName], Maybe FOL)

data BExpr = BoolConst Bool
  | Negate BExpr
  | BBinary BoolOp BExpr BExpr
  | RBinary ROp AExpr AExpr
  deriving (Show, Eq)

data BoolOp = Conj | Disj
  deriving (Show, Eq)

data ROp = Less | Eq | Greater
  deriving (Show, Read, Eq)

data AExpr = Var VName
  | Ghost VName
  | IntConst Integer
  | Neg AExpr
  | ABinary ArithOp AExpr AExpr
  deriving (Show, Eq)

data ArithOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show, Enum)

data Stmt = Seq Stmt Stmt
  | GhostAss VName AExpr
  | Assign VName AExpr
  | If BExpr Stmt Stmt
  | Asst FOL
  | While BExpr FOL (Maybe Variant) Stmt
  | Skip
  | Fail
  deriving (Show, Eq)

type Variant = AExpr

data FOL = Cond BExpr
  | Forall VName FOL
  | Exists VName FOL
  | ANegate FOL
  | AConj FOL FOL
  | ADisj FOL FOL
  | AImp FOL FOL
  deriving (Show, Eq)

type VName = String

