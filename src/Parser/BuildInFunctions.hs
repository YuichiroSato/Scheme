module Parser.BuildInFunctions
  ( isBuildIn
  , isArithmeticFunction
  , isComparisionFunction
  , isLogicFunction
  , isListOperator
  ) where

buildInFunctions :: [String]
buildInFunctions = arithmeticFunctions ++ comparisionFunctions ++ logicFunctions ++ listOperators

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

logicFunctions :: [String]
logicFunctions =
  [ "and"
  , "or"
  , "not"
  , "eq"]

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

isLogicFunction :: String -> Bool
isLogicFunction str = elem str logicFunctions

isListOperator :: String -> Bool
isListOperator str = elem str listOperators

isBuildIn :: String -> Bool
isBuildIn str = elem str buildInFunctions 
