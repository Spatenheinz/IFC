module Pretty where
import AST

prettyF :: FOL -> Int -> String
prettyF (Cond b) i = prettyB b
prettyF (Forall x a) i = "forall " <> x <> ".\n" <> replicate i ' ' <> prettyF a (i + 4)
prettyF (Exists x a) i = "exists " <> x <> ".\n" <> replicate i ' ' <> prettyF a (i + 4)
prettyF (ANegate (Cond (RBinary Less a b))) i = "(" <> prettyA a <> " >= " <> prettyA b <> ")"
prettyF (ANegate (Cond (RBinary Greater a b))) i = "(" <> prettyA a <> " <= " <> prettyA b <> ")"
prettyF (ANegate (Cond (RBinary Eq a b))) i = "(" <> prettyA a <> " / " <> prettyA b <> ")"
prettyF (ANegate a) i = "~" <> "(" <> prettyF a i <> ")"
prettyF (AConj a b) i = "(" <> prettyF a i <> " /\\ " <> prettyF b i <> ")"
prettyF (ADisj a b) i = "(" <> prettyF a i <> " \\/ " <> prettyF b i <> ")"
prettyF (AImp a b) i = prettyF a i <> " => " <> prettyF b i

prettyB :: BExpr -> String
prettyB (BoolConst b) = show b
prettyB (Negate (RBinary Less a b)) = "(" <> prettyA a <> " >= " <> prettyA b <> ")"
prettyB (Negate (RBinary Greater a b)) = "(" <> prettyA a <> " <= " <> prettyA b <> ")"
prettyB (Negate (RBinary Eq a b)) = "(" <> prettyA a <> " / " <> prettyA b <> ")"
prettyB (Negate a) = "~" <> "(" <> prettyB a <> ")"
prettyB (BBinary op a b) = prettyB a <> prettyBOp op <> prettyB b
prettyB (RBinary op a b) = prettyA a <> prettyROp op <> prettyA b

prettyA :: AExpr -> String
prettyA (Var x) = x
prettyA (Ghost x) = x
prettyA (IntConst i) = show i
prettyA (Neg a) = "-" <> show a
prettyA (ABinary op a b) = prettyA a <> prettyAOp op <> prettyA b

prettyBOp :: BoolOp -> String
prettyBOp Conj = " && "
prettyBOp Disj = " || "

prettyROp :: ROp -> String
prettyROp Less = " < "
prettyROp Eq = " = "
prettyROp Greater = " > "

prettyAOp :: ArithOp -> String
prettyAOp Add = " + "
prettyAOp Sub = " - "
prettyAOp Mul = " * "
prettyAOp Mod = " % "
prettyAOp Div = " / "
