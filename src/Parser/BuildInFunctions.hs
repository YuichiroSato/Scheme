module Parser.BuildInFunctions
  ( isBuildIn ) where

buildInFunctions :: [String]
buildInFunctions =
  [ "+"
  , "-"
  , "*"
  , "/"
  , "mod"
  , "<"
  , ">"
  , "<="
  , ">="
  , "="
  , "cons"
  , "list"
  , "car"
  , "cdr"
  , "null?"]

isBuildIn :: String -> Bool
isBuildIn str = elem str buildInFunctions 
