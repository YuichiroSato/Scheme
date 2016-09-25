module Parser.RoughParser
  ( parseRough ) where

import Control.Applicative((<$>))
import Text.Parsec(eof, Parsec, parse, ParseError, (<|>))
import Text.Parsec.Char(anyChar, char, noneOf, oneOf, spaces, string)
import Text.Parsec.Combinator(many1)
import Text.Parsec.Prim(try)
import Data.ParseTree(ParseTree(..), Exp(..), Symbol(..), Val(..))
import Parser.SpecialForms(isSpecialForm)

parseRough :: String -> Either ParseError ParseTree
parseRough = parse top ""

top :: Parsec String () ParseTree
top = do
  _ <- spaces
  pt <- try $ (char '(' *> parsePlain) <|> (char '#' *> parseSharp) <|> (char '\'' *> parseQuote) <|> (char '`' *> parseBackQuote) <|> parseAtom
  return pt

parsePlain :: Parsec String () ParseTree
parsePlain = try $ parseEmptyList <|> parsePlainTree

parseEmptyList :: Parsec String () ParseTree
parseEmptyList = do
  _ <- char ')'
  return $ Plain $ ValExp $ EmptyList

parsePlainTree :: Parsec String () ParseTree
parsePlainTree = Plain <$> plainParser

plainParser :: Parsec String () Exp
plainParser = try $ parseExpressions <|> parseVal <|> parseSymbol

parseSharp :: Parsec String () ParseTree
parseSharp = makeSharp <$> top 
  where
    makeSharp (Plain e) = Sharp e
    makeSharp _ = EmptyTree

parseQuote :: Parsec String () ParseTree
parseQuote = makeQuote <$> top
  where
    makeQuote (Plain e) = Quote e
    makeWuote _ = EmptyTree

parseBackQuote :: Parsec String () ParseTree
parseBackQuote = makeBackQuote <$> top
  where
    makeBackQuote (Plain e) = BackQuote e
    makeBackQuote _ = EmptyTree

parseAtom :: Parsec String () ParseTree
parseAtom = Plain <$> (try $ parseVal <|> parseSymbol)

parseVal :: Parsec String () Exp
parseVal = ValExp <$> (try $ (try parseDouble) <|> (try parseInt) <|> parseString)

parseSymbol :: Parsec String () Exp
parseSymbol =  makeExp <$> symbolParser
  where
    makeExp v = if isSpecialForm v
                  then SymbolExp $ Special v
                  else SymbolExp $ Variable v

symbolParser :: Parsec String () String
symbolParser = many1 $ noneOf "( )"

parseExpressions :: Parsec String () Exp
parseExpressions = do
  ps <- many1 $ try (top <* char ' ')
  p <- top <* char ')'
  return $ Exps $ ps ++ [p]

parseInt :: Parsec String () Val
parseInt = IntVal <$> intParser

-- TODO: parse +-
intParser :: Parsec String () Int
intParser = do
  i <- many1 $ oneOf "1234567890"
  return $ read i

parseDouble :: Parsec String () Val
parseDouble = DoubleVal <$> doubleParser

-- TODO: parse +-
doubleParser :: Parsec String () Double
doubleParser = do
  d1 <- many1 $ oneOf "1234567890"
  _ <- char '.'
  d2 <- many1 $ oneOf "1234567890"
  return $ read $ (d1 ++ "." ++ d2)

parseChar :: Parsec String () Exp
parseChar = ValExp . CharVal <$> charParser

charParser :: Parsec String () Char
charParser = do
  _ <- char '\\'
  c <- anyChar
  return c

parseString :: Parsec String () Val
parseString = StringVal <$> stringParser

-- TODO: parse \"
stringParser :: Parsec String () String
stringParser = do
  _ <- char '"'
  str <- many1 $ noneOf " \""
  _ <- char '"'
  return str

parseBool :: Parsec String () Exp
parseBool = ValExp . BoolVal <$> boolParser

boolParser :: Parsec String () Bool
boolParser = do
  b <- char 't' <|> char 'f'
  return $ b == 't'
