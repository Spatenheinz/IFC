import WLP
import AST
import Data.SBV (ThmResult)
import Pretty
main :: IO ()
main = testOfMul

testOfMul :: IO ()
testOfMul = putStrLn $ prettyF thing 0
  where thing =
         Forall "q#3" ((Cond (RBinary Eq (Var "q#3") (IntConst 10))) .=>.
          Forall "r#1" ((Cond (RBinary Eq (Var "r#1") (IntConst 55))) .=>.
                ((ANegate (Cond (RBinary Less (Var "q#3") (IntConst 0))))
                ./\. ANegate (Cond (RBinary Less (Var "r#1") (IntConst 0))))
                ./\. Forall "res#3" ((Cond (RBinary Eq (Var "res#3") (IntConst 0))) .=>.
                Forall "ghosta" ((Cond (RBinary Eq (Var "ghosta") (Var "q#3"))) .=>.
                (Cond (RBinary Eq (Var "res#3") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#3")) (Var "r#1"))))
                ./\. Forall "q#1" (Forall "res#1" ((((Cond (RBinary Greater (Var "q#1") (IntConst 0))) ./\. (Cond (RBinary Eq (Var "res#1") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#1")) (Var "r#1")))))
                        .=>. Forall "res#2" ((Cond (RBinary Eq (Var "res#2") (ABinary Add (Var "res#1") (Var "r#1"))))
                        .=>. Forall "q#2" ((Cond (RBinary Eq (Var "q#2") (ABinary Sub (Var "q#1") (IntConst 1))))
                        .=>. (Cond (RBinary Eq (Var "res#2") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#2")) (Var "r#1")))))))
                        ./\. ((ANegate (Cond (RBinary Greater (Var "q#1") (IntConst 0)))) ./\. (Cond (RBinary Eq (Var "res#1") (ABinary Mul (ABinary Sub (Var "ghosta") (Var "q#1")) (Var "r#1"))))
                        .=>. (Cond (RBinary Eq (Var "res#1") (ABinary Mul (Var "ghosta") (Var "r#1")))))))))))
