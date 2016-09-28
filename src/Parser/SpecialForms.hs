module Parser.SpecialForms 
  ( isSpecialForm ) where

specialForms :: [String]
specialForms =
  [ "if"
  , "cond"
  , "else"
  , "let"
  , "define"
  , "lambda"
  , "set!"]

isSpecialForm :: String -> Bool
isSpecialForm str = elem str specialForms
