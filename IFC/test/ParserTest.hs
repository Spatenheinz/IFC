{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module ParserTest where

import AST
import Parser (parseString)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (===))
import Pretty (prettyProgram, prettyHeader)
import QCInstances
import CodeBlocks

testParseOK :: String -> String -> Stmt -> TestTree
testParseOK desc con s =
  testCase desc $ assertEqual ("Parsing: " ++ con) (Right (s,([],Nothing)))
    (parseString (headerStr <> con))

testParseBad :: String -> String -> TestTree
testParseBad name s =
  testCase ("*" ++ name) $
    assertEqual ("Parsing: " ++ show s)
      (Left msg)
      (case parseString s of Left _ -> Left msg; Right p -> Right p)


header = ([], Nothing)
headerStr = prettyHeader header

msg = "<message>"

lex = testGroup "Parsing" [
  testGroup "lexical" [
  testGroup "Assignments" [
      testParseOK "simple" "x := 10;" (Assign "x" (IntConst 10)),
      testParseOK "simple binary" "x := 0b1;" (Assign "x" (IntConst 1)),
      testParseOK "simple hex" "x := 0x0f;" (Assign "x" (IntConst 15)),
      testParseOK "simple octal" "x := 0o10;" (Assign "x" (IntConst 8)),
      testParseOK "simple negative" "x := -10;" (Assign "x" (Neg $ IntConst 10)),
      testParseOK "simple negative binary" "x := -0b1;" (Assign "x" (Neg $ IntConst 1)),
      testParseOK "simple negative hex" "x := -0x0f;" (Assign "x" (Neg $ IntConst 15)),
      testParseOK "simple negative octal" "x := -0o10;" (Assign "x" (Neg $ IntConst 8)),
      testParseOK "complex name" "x___BCD := 10;" (Assign "x___BCD" (IntConst 10)),
      testParseOK "complex name _ start" "_x___BCD := 10;" (Assign "_x___BCD" (IntConst 10)),
      testParseBad "complex name capital start" "A___BCD := 10;",
      testParseBad "keyword" "if := 10;",
      testParseBad "complex name capital start" "A___BCD := 10;",
      testParseOK "name end with keyword" "has_if := 10;" (Assign "has_if" (IntConst 10)),
      testParseOK "name starts with keyword" "if_ := 10;" (Assign "if_" (IntConst 10)),
      testParseBad "incomplete assignment" "x",
      testParseBad "Missing delimiter" "x := 10",
      testParseOK "violate" "violate;" Fail,
      testParseOK "skip" "skip;" Skip
      ]
  ],
  testGroup "Syntactical" [
      testParseOK "if c then x ~ if c then x else skip" "if x > 10 { violate; };"
        (If (RBinary Greater (Var "x") (IntConst 10)) Fail Skip)
  ],
  testGroup "Ambiguity" [
      testParseOK "a + 4 * 3 - 1" "v := a + 4 * 3 - 1;"
       (Assign "v" (ABinary Sub (ABinary Add (Var "a") (ABinary Mul (IntConst 4) (IntConst 3))) (IntConst 1)))
      ]
  , qc
  ]
qc = localOption (mkTimeout 10000000) $ testGroup "QC" [
  testProperty "Program" $ \(s :: stmt) ->
      parseString (prettyProgram s header) === Right (s, header)
  ]
-- Asst (Cond (BBinary Conj (BoolConst False) (BoolConst False)))
