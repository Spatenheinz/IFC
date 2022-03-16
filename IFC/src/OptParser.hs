module OptParser (parseStore) where

import Text.Megaparsec
import Data.Function (on)
import Control.Applicative (liftA2)
import AST
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import Data.Void

type Parser = Parsec Void String

parseStore :: String -> Either String [(VName, Integer)]
parseStore s = case parse (brackets inputP) "" s of
                 Left e -> Left "Input list not wellformed"
                 Right r -> return r

inputP :: Parser [(VName, Integer)]
inputP = sepBy (parens pairP) (symbol ",")
  where pairP = identP >>= \v -> symbol "," >> integer >>= \i -> return (v,i)

identP :: Parser String
identP = (lexeme . try) ident
  where
    ident = liftA2 (:) identH $ many (identH <|> digitChar)
    identH = letterChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = (between `on` symbol) "(" ")"

brackets :: Parser a -> Parser a
brackets = (between `on` symbol) "[" "]"

integer :: (Num a) => Parser a
integer = lexeme L.decimal <|> L.binary <|> L.octal <|> L.hexadecimal
