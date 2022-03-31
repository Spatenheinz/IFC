{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module ParserTest where

import AST
import Parser (parseString)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
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
      testParseOK "complex name" "x___BCD := 10;" (Assign "x___BCD" (IntConst 10)),
      testParseOK "complex name _ start" "_x___BCD := 10;" (Assign "_x___BCD" (IntConst 10)),
      testParseBad "complex name capital start" "A___BCD := 10;",
      testParseBad "keyword" "if := 10;",
      testParseBad "complex name capital start" "A___BCD := 10;",
      testParseOK "name end with keyword" "has_if := 10;" (Assign "has_if" (IntConst 10)),
      testParseOK "name starts with keyword" "if_ := 10;" (Assign "if_" (IntConst 10)),
      testParseBad "incomplete assignment" "x",
      testParseBad "Missing delimiter" "x := 10"
      ],
  testGroup "Numbers" [
      testParseOK "decimal" "x := 42;" (Assign "x" (IntConst 42)),
      testParseOK "binary" "x := 0b1;" (Assign "x" (IntConst 1)),
      testParseOK "hex" "x := 0x0f;" (Assign "x" (IntConst 15)),
      testParseOK "octal" "x := 0o10;" (Assign "x" (IntConst 8)),
      testParseOK "decimal negative" "x := -10;" (Assign "x" (Neg $ IntConst 10)),
      testParseOK "binary negative" "x := -0b1;" (Assign "x" (Neg $ IntConst 1)),
      testParseOK "hex negative" "x := -0x0f;" (Assign "x" (Neg $ IntConst 15)),
      testParseOK "octal negative" "x := -0o10;" (Assign "x" (Neg $ IntConst 8))
                      ],
  testGroup "Ghosts" [
      testParseOK "ghost emoji" "👻a := x;" (GhostAss "👻a" (Var "x")),
      testParseOK "ghost $" "$a := x;" (GhostAss "👻a" (Var "x"))
                     ],
  testGroup "Skip" [
      testParseOK "skip" "skip;" Skip
                 ],
  testGroup "Fail" [
      testParseOK "violate" "violate;" Fail
                 ]
  ],
  testGroup "Syntactical" [
      testParseOK "if c then x ~ if c then x else skip" "if x > 10 { violate; };"
        (If (RBinary Greater (Var "x") (IntConst 10)) Fail Skip),
      testParseOK "forall x y z" "#{forall x y z. true};" (Asst $ Forall "x" (Forall "y" (Forall "z" $ Cond $ BoolConst True)))
      ],
  testGroup "Ambiguity" [
      testParseOK "a + 4 * 3 - 1" "v := a + 4 * 3 - 1;"
       (Assign "v" (ABinary Sub (ABinary Add (Var "a") (ABinary Mul (IntConst 4) (IntConst 3))) (IntConst 1)))
      -- ~~~ should be ~(~(~))
      ]
  , qc
  ]
qc = localOption (mkTimeout 4000000) $ testGroup "QC" [
  testProperty "Program" $ \(s :: stmt) ->
      parseString (prettyProgram s header) === Right (s, header)
  ]
-- Asst (Cond (BBinary Conj (BoolConst False) (BoolConst False)))
