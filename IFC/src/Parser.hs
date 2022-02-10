module Parser where

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           AST
import           Control.Applicative            (liftA2)
import           Control.Monad                  (void)
import           Data.Function                  (on)
import           Data.Void
type Parser = Parsec Void String


sc :: Parser ()
sc = L.space (void spaceChar) lc bc
  where lc = L.skipLineComment "--"
        bc = L.skipBlockComment "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = (between `on` symbol) "(" ")"

cbrackets :: Parser a -> Parser a
cbrackets = (between `on` symbol) "{" "}"

integer :: (Num a) => Parser a
integer = lexeme L.decimal <|> L.binary <|> L.octal <|> L.hexadecimal

sep :: Parser String
sep = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

keywords :: [String]
keywords = ["if", "then", "else", "while", "do", "violate", "skip", "true", "false", "not"]

identP :: Parser String
identP = (lexeme . try) (ident >>= notRword) <?> "identifier"
  where
    ident = liftA2 (:) identH $ many (identH <|> digitChar)
    identH = letterChar <|> char '_'
    notRword i
      | i `elem` keywords = fail $ "keyword " ++ show i ++ "used as an identifier"
      | otherwise = return i

-- A program is simply a statement
parseString :: String -> IO ()
parseString s = case parse (between sc eof programP) "" s of
         Left bundle -> putStrLn (errorBundlePretty bundle)
         Right xs    -> print xs

-- More specifically we want the toplevel to be a sequence of statements.
-- This makes stuff slightly easier
programP :: Parser Stmt
programP = parens programP <|> seqP

seqP :: Parser Stmt
seqP = Seq <$> stmtP `sepBy1` sep

stmtP :: Parser Stmt
stmtP = ifP <|> whileP <|> skipP <|> defP <|> violateP

defP :: Parser Stmt
defP = do
  vname <- identP
  void $ symbol ":="
  Def vname <$> aExprP

ifP :: Parser Stmt
ifP = do
  rword "if"
  c <- bExprP
  stmt1 <- cbrackets stmtP
  If c stmt1 <$> option Skip else'
  where
    else' = rword "else" *> cbrackets stmtP

whileP :: Parser Stmt
whileP = do
  rword "while"
  c <- bExprP
  While c <$> cbrackets stmtP

skipP :: Parser Stmt
skipP = Skip <$ rword "skip"

violateP :: Parser Stmt
violateP = Skip <$ rword "violate"

aExprP :: Parser AExpr
aExprP = makeExprParser aTermP operators
  where operators =
                [ [ Prefix (Neg <$ symbol "-")
                ]
                , [ InfixL (ABinary Mul <$ symbol "*")
                  , InfixL (ABinary Div <$ symbol "/")
                  ]
                , [ InfixL (ABinary Add <$ symbol "+")
                  , InfixL (ABinary Sub <$ symbol "-")
                  ]
                ]

bExprP :: Parser BExpr
bExprP = makeExprParser bTermP operators
  where operators =
                [ [ Prefix (Negate <$ rword "!")
                ]
                , [ InfixL (BBinary Conj <$ symbol "&&")
                  , InfixL (BBinary Disj <$ symbol "||")
                  ]
                ]

aTermP :: Parser AExpr
aTermP =
  parens aExprP
  <|> Var <$> identP
  <|> IntConst <$> integer

bTermP :: Parser BExpr
bTermP =
  parens bExprP
  <|> (BoolConst True <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> relative
  where relative = aExprP >>= \a0 -> relationP >>= \r -> r a0 <$> aExprP

relationP :: Parser (AExpr -> AExpr -> BExpr)
relationP = choice [ symbol "==" >> return (RBinary Eq)
                   , symbol "!=" >> return (notOp Eq)
                   , try $ symbol ">=" >> return (notOp Less)
                   , try $ symbol "<=" >> return (notOp Greater)
                   , symbol "<"  >> return (RBinary Less)
                   , symbol ">"  >> return (RBinary Greater)
                   ]
  -- Just some helpers to make the code look beautiful
  where (.....) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
        (.....) = (.).(.).(.)
        notOp = Negate ..... RBinary
