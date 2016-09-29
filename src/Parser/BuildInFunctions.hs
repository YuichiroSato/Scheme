module Parser.BuildInFunctions
  ( isBuildIn ) where

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
conparisionFunctions =
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


isBuildIn :: String -> Bool
isBuildIn str = elem str buildInFunctions 
