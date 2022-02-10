module AST where

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
  | IntConst Integer
  | Neg AExpr
  | ABinary ArithOp AExpr AExpr
  deriving (Show)

data ArithOp = Add | Sub | Mul | Div | Mod
  deriving (Show)

data Stmt = Seq [Stmt]
  | Def VName AExpr
  | If BExpr Stmt Stmt
  | While BExpr Stmt
  | Skip
  | Violate
  deriving (Show)


type VName = String
