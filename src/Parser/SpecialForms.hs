module Parser.SpecialForms 
  ( isSpecialForm ) where

specialForms :: [String]
specialForms =
  [ "if"
  , "define"
  , "lambda"]

isSpecialForm :: String -> Bool
isSpecialForm str = elem str specialForms
