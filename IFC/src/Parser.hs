{-# LANGUAGE LambdaCase #-}

module Parser where

import           Control.Monad.Combinators.Expr
import Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import           AST
import           Control.Applicative            (liftA2)
import           Control.Monad                  (void)
import           Data.Function                  (on)
import           Data.Void
import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Utils

type Parser = ParsecT Void String (ReaderT Bool (StateT Header Identity))

parseString :: String -> Either String (Stmt, Header)
parseString s = case runIdentity
                     $ runStateT
                     (runReaderT (runParserT (between sc eof programP) "" s) False)
                     ([],Nothing) of
         (Left bundle, _) -> Left $ errorBundlePretty bundle
         (Right xs, st) -> return (xs, st)

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

brackets :: Parser a -> Parser a
brackets = (between `on` symbol) "[" "]"

cbrackets :: Parser a -> Parser a
cbrackets = (between `on` symbol) "{" "}"

signed = L.signed (return ()) integer

integer :: (Num a) => Parser a
integer = lexeme $ (string "0" >>
                    (string "b" *> L.binary) <|>
                    (string "x" *> L.hexadecimal) <|>
                    (string "o" *> L.octal) <|> return 0)
                    <|> L.decimal

sep :: Parser String
sep = symbol ";"

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

keywords :: [String]
keywords = ["if", "then", "else", "while", "forall", "violate", "skip", "true", "false"]

identP :: Parser String
identP = (lexeme . try) (ident >>= notRword) <?> "identifier"
  where
    ident = liftA2 (:) identH $ many (letterChar <|> digitChar <|> char '_')
    identH = lowerChar <|> char '_'
    notRword i
      | i `elem` keywords = fail $ "keyword " ++ show i ++ "used as an identifier"
      | otherwise = return i

ghostAssP :: Parser Stmt
ghostAssP = do
  vname <- ghostidP
  void $ symbol ":="
  GhostAss vname <$> aExprP

ghostidP :: Parser String
ghostidP = string "👻" >> ("👻"<>) <$> identP

varP :: Parser AExpr
varP = do
  ask >>= \case
    True -> Ghost <$> ghostidP <|> Var <$> identP
    False -> Var <$> identP

programP :: Parser Stmt
programP = preconds >> option Skip seqP

preconds :: Parser ()
preconds = do
  symbol "vars:"
  vs <- brackets (sepBy identP (symbol ","))
  symbol "requirements:"
  req <- cbrackets (option Nothing (Just <$> impP))
  symbol "<!=_=!>"
  modify (const (vs,req))


seqP :: Parser Stmt
seqP = do
  xs <- stmtP `endBy1` sep
  return $ foldr1 Seq xs

stmtP :: Parser Stmt
stmtP = asstP <|> assignP <|> ifP <|> whileP <|> skipP <|> violateP <|> ghostAssP

asstP :: Parser Stmt
asstP = do
  void $ symbol "#"
  Asst <$> cbrackets (local (const True) quantP)

quantP :: Parser FOL
quantP = quant <|> impP
  where quant = (on eitherP (\x -> between x (symbol ".") (some identP)) `on` rword)
                  "forall" "exists" >>= \case
                      Left [fa] -> basef fa
                      Left fas -> fold_ (fmap . Forall) basef fas
                      Right [ex] -> basee ex
                      Right exs ->  fold_ (\x a -> Exists x <$> a) basee exs
        basef x = Forall x <$> quantP
        basee x = Exists x <$> quantP
        fold_ f b xs = foldr f (b $ last xs) (init xs)

impP :: Parser FOL
impP = do
  a0 <- cdP
  option a0 (symbol "=>" >> aimp a0 <$> impP)

cdP :: Parser FOL
cdP = negPreP >>= cdOptP

cdOptP :: FOL -> Parser FOL
cdOptP a0 = option a0 (do
            op <- cdchoiceP
            a1 <- cdP
            return (a0 `op` a1))

cdchoiceP :: Parser (FOL -> FOL -> FOL)
cdchoiceP = choice [ symbol "/\\" >> return aconj
                   , symbol "\\/" >> return adisj
                   ]

negPreP :: Parser FOL
negPreP = (symbol "~" >> anegate <$> negPreP) <|> topP

topP :: Parser FOL
topP = ask >>= \case
  True -> try (Cond <$> bTermP) <|> parens quantP
  False -> try (Cond <$> bTermP) <|> parens impP

assignP :: Parser Stmt
assignP = do
  vname <- identP
  void $ symbol ":="
  Assign vname <$> aExprP

ifP :: Parser Stmt
ifP = do
  rword "if"
  c <- bExprP
  stmt1 <- cbrackets seqP
  If c stmt1 <$> option Skip else'
  where
    else' = rword "else" *> cbrackets seqP

whileP :: Parser Stmt
whileP = do
  rword "while"
  c <- bExprP
  invs <- sepBy (symbol "?" >> local (const True) (cbrackets quantP)) (symbol ";")
  var <- option Nothing (symbol "!" >> Just <$> cbrackets aExprP)
  While c invs var <$> cbrackets seqP

skipP :: Parser Stmt
skipP = Skip <$ rword "skip"

violateP :: Parser Stmt
violateP = Fail <$ rword "violate"

aExprP :: Parser AExpr
aExprP = makeExprParser aTermP operators
  where operators =
                [ [ Prefix (Neg <$ symbol "-")
                ]
                , [ InfixL (abinary Mul <$ symbol "*")
                  , InfixL (abinary Div <$ try (do s <- symbol "/"
                                                   void $ notFollowedBy $ symbol "\\" <|> symbol "="
                                                   return s))
                  , InfixL (abinary Mod <$ symbol "%")
                  ]
                , [ InfixL (abinary Add <$ symbol "+")
                  , InfixL (abinary Sub <$ symbol "-")
                  ]
                ]

bExprP :: Parser BExpr
bExprP = makeExprParser bTermP operators
  where operators =
                [ [ Prefix (bnegate <$ rword "!")
                ]
                , [ InfixL (bconj <$ symbol "&&")
                  , InfixL (bdisj <$ symbol "||")
                  ]
                ]

aTermP :: Parser AExpr
aTermP =
     parens aExprP
    <|> varP
    <|> IntConst <$> integer
    <?> "aterm"

bTermP :: Parser BExpr
bTermP =
  try relative <|>
  parens bExprP
  <|> BoolConst True <$ rword "true"
  <|> BoolConst False <$ rword "false"
  where relative = aExprP >>= \a0 -> relationP >>= \r -> r a0 <$> aExprP

relationP :: Parser (AExpr -> AExpr -> BExpr)
relationP = choice [ symbol "=" >> return (RBinary Eq)
                   , symbol "/=" >> return (notOp Eq)
                   , try $ symbol ">=" >> return (notOp Less)
                   , try $ symbol "<=" >> return (notOp Greater)
                   , symbol "<"  >> return (RBinary Less)
                   , symbol ">"  >> return (RBinary Greater)
                   ]
  -- Just some helpers to make the code look beautiful
  where notOp = Negate ..... RBinary
