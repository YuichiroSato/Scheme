module Parser.BuildInFunctions
  ( isBuildIn
  , isArithmeticFunction
  , isComparisionFunction
  , isListOperator
  ) where

buildInFunctions :: [String]
buildInFunctions = arithmeticFunctions ++ comparisionFunctions ++ listOperators

arithmeticFunctions :: [String]
arithmeticFunctions =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "mod"]
 
comparisionFunctions :: [String]
comparisionFunctions =
  [ "<"
  , ">"
  , "<="
  , ">="
  , "="]

listOperators :: [String]
listOperators =
  [ "cons"
  , "list"
  , "car"
  , "cdr"
  , "null?"]

isArithmeticFunction :: String -> Bool
isArithmeticFunction str = elem str arithmeticFunctions

isComparisionFunction :: String -> Bool
isComparisionFunction str = elem str comparisionFunctions

isListOperator :: String -> Bool
isListOperator str = elem str listOperators

isBuildIn :: String -> Bool
isBuildIn str = elem str buildInFunctions 
