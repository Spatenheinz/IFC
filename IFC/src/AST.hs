module AST where

type Header = ([VName], Maybe FOL)

data BExpr = BoolConst Bool
  | Negate BExpr
  | BBinary BoolOp BExpr BExpr
  | RBinary ROp AExpr AExpr
  deriving (Show, Eq)

data BoolOp = Conj | Disj
  deriving (Show, Eq)

bnegate :: BExpr -> BExpr
bnegate (Negate (Negate b)) = b
bnegate (BoolConst True) = BoolConst False
bnegate (BoolConst False) = BoolConst True
bnegate b = Negate b
-- should we also have for false?
bconj :: BExpr -> BExpr -> BExpr
bconj b1 b2 = case (b1,b2) of
                (BoolConst True, _) -> b2
                (_, BoolConst True) -> b1
                (_,_) -> BBinary Conj b1 b2

-- should we also have for true
bdisj :: BExpr -> BExpr -> BExpr
bdisj b1 b2 = case (b1,b2) of
                (BoolConst False, _) -> b2
                (_, BoolConst False) -> b1
                (_,_) -> BBinary Disj b1 b2

eq :: AExpr -> AExpr -> BExpr
eq a b | a == b = RBinary Eq a b
eq a b = RBinary Eq a b

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

abinary :: ArithOp -> AExpr -> AExpr -> AExpr
abinary Add (IntConst 0) a = a
abinary Add a (IntConst 0) = a
abinary Sub (IntConst 0) a = Neg a
abinary Sub a (IntConst 0) = a
abinary Mul (IntConst 0) _ = IntConst 0
abinary Mul _ (IntConst 0) = IntConst 0
abinary Mul (IntConst 1) a = a
abinary Mul a (IntConst 1) = a
abinary Div a (IntConst 1) = a
abinary Mod _ (IntConst 1) = IntConst 0
abinary o a1 a2 = ABinary o a1 a2

data Stmt = Seq Stmt Stmt
  | GhostAss VName AExpr
  | Assign VName AExpr
  | If BExpr Stmt Stmt
  | Asst FOL
  | While BExpr FOL [Variant] Stmt
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

anegate :: FOL -> FOL
anegate (ANegate (ANegate a)) = a
anegate (Cond (BoolConst True)) = ffalse
anegate (Cond (BoolConst False)) = ftrue
anegate a = ANegate a


-- would be nice if this could be a pattern
ffalse :: FOL
ffalse = Cond $ BoolConst False

ftrue :: FOL
ftrue = Cond $ BoolConst True

aconj :: FOL -> FOL -> FOL
aconj f1 f2 = case (f1, f2) of
  (Cond (BoolConst False), _) -> ffalse
  (_, Cond (BoolConst False)) -> ffalse
  (_, Cond (BoolConst True)) -> f1
  (Cond (BoolConst True), _) -> f2
  _ -> AConj f1 f2

adisj :: FOL -> FOL -> FOL
adisj f1 f2 = case (f1, f2) of
  (Cond (BoolConst False), _) -> f2
  (_, Cond (BoolConst False)) -> f1
  _ -> ADisj f1 f2

aimp :: FOL -> FOL -> FOL
aimp (Cond (Negate (RBinary Eq (IntConst a) (IntConst b))))  f | a /= b = f
aimp (Cond (RBinary Eq (IntConst a) (IntConst b)))  _ | a /= b = ftrue
aimp f1 f2 = case (f1, f2) of
  (Cond (BoolConst True), _) -> f2
  (Cond (BoolConst False), _) -> ftrue
  (Cond (RBinary Eq (IntConst 0) (IntConst 0)), Cond (BoolConst False)) -> ffalse
  (Cond (Negate (RBinary Eq (IntConst 0) (IntConst 0))), _) -> f2
  _ -> AImp f1 f2

type VName = String

(./\.) :: FOL -> FOL -> FOL
(./\.) = aconj
infixr 3 ./\.
(.\/.) :: FOL -> FOL -> FOL
(.\/.) = adisj
infixr 2 .\/.
(.=>.) :: FOL -> FOL -> FOL
(.=>.) = aimp
infixr 1 .=>.
