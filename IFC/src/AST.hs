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

data Stmt = Seq Stmt Stmt
  | Def  VName AExpr
  | If BExpr Stmt Stmt
  | Asst FOL
  | While BExpr Stmt
  | Skip
  | Fail
  deriving (Show)

data FOL = Cond BExpr
  | Forall VName FOL
  | ANegate FOL
  | AOp BoolOp FOL FOL
  deriving (Show)

-- x = a;
-- if cond {};
-- !{};
-- While cond {};
-- skip;
-- ship;
-- skip;

-- Seq Def.. Seq If Seq Assertion Seq While $ Seq skip $ Seq Ship Skip

-- [Def, If, Assert, While, Skip]

-- Seq Def $ Seq If $ Seq Assert $ Seq While Skip

-- foldr Seq (last xs) init xs

-- wp (Seq stmt1 stmt2) prev = wp stmt1 (wp stmt2 prev)

type VName = String
