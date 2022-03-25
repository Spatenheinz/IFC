module Pretty where
import AST
import Data.Char (toLower)

prettyProgram :: Stmt -> Header -> String
prettyProgram s h = prettyHeader h <> prettySt s

prettyHeader :: ([VName], Maybe FOL) -> String
prettyHeader (xs, req) = "vars: " <> show xs <> "\nrequirements: " <> ppr <> "\n<!=_=!>\n"
  where ppr = case req of
                Nothing -> "{}"
                Just a -> "{" <> prettyF a 0 <> "}"

prettySt :: Stmt -> String
prettySt (Seq s1 s2) = prettySt s1 <> prettySt s2
prettySt (GhostAss x a) = x <> " := " <> prettyA a <> ";\n"
prettySt (Assign x a) = x <> " := " <> prettyA a <> ";\n"
prettySt (If b s1 s2) = "if " <> prettyB b <> " {\n" <> prettySt s1 <> "} else {\n" <> prettySt s2 <> "};"
prettySt (Asst f) = "#{" <> prettyF f 0 <> "};\n"
prettySt (While b invs var s) = "while " <> prettyB b <> "?{" <> prettyF invs' 0 <> "} " <> var'
  <> "{\n" <> prettySt s <> "};\n"
  where var' = case var of
                 Just a -> "!{" <> prettyA a <> "}"
                 Nothing -> ""
        invs' = foldr (./\.) (Cond $ BoolConst True) invs
prettySt Skip = "skip;"
prettySt Fail = "violate;"

prettyF :: FOL -> Int -> String
prettyF (Cond b) i = prettyB b
prettyF (Forall x a) i = "forall " <> x <> ".\n" <> replicate i ' ' <> prettyF a (i + 4)
prettyF (Exists x a) i = "exists " <> x <> ".\n" <> replicate i ' ' <> prettyF a (i + 4)
prettyF (ANegate (Cond (RBinary Less a b))) i = "(" <> prettyA a <> " >= " <> prettyA b <> ")"
prettyF (ANegate (Cond (RBinary Greater a b))) i = "(" <> prettyA a <> " <= " <> prettyA b <> ")"
prettyF (ANegate (Cond (RBinary Eq a b))) i = "(" <> prettyA a <> " /= " <> prettyA b <> ")"
prettyF (ANegate a) i = "~" <> "(" <> prettyF a i <> ")"
prettyF (AConj a b) i = "(" <> prettyF a i <> " /\\ " <> prettyF b i <> ")"
prettyF (ADisj a b) i = "(" <> prettyF a i <> " \\/ " <> prettyF b i <> ")"
prettyF (AImp a b) i = "(" <> prettyF a i <> " => " <> prettyF b i <> ")"

prettyB :: BExpr -> String
prettyB (BoolConst b) = map toLower $ show b
prettyB (Negate (RBinary Less a b)) = "(" <> prettyA a <> " >= " <> prettyA b <> ")"
prettyB (Negate (RBinary Greater a b)) = "(" <> prettyA a <> " <= " <> prettyA b <> ")"
prettyB (Negate (RBinary Eq a b)) = "(" <> prettyA a <> " /= " <> prettyA b <> ")"
prettyB (Negate a) = "~" <> "(" <> prettyB a <> ")"
prettyB (BBinary op a b) = prettyB a <> prettyBOp op <> prettyB b
prettyB (RBinary op a b) = prettyA a <> prettyROp op <> prettyA b

prettyA :: AExpr -> String
prettyA (Var x) = x
prettyA (Ghost (x:xs)) = '👻':xs
prettyA (IntConst i) = show i
prettyA (Neg a) = "(-" <> prettyA a <> ")"
prettyA (ABinary op a b) = "(" <> prettyA a <> prettyAOp op <> prettyA b <> ")"

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
