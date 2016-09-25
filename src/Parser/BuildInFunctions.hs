module Parser.BuildInFunctions
  ( isBuildIn ) where

buildInFunctions :: [String]
buildInFunctions = ["+", "-", "*", "/", "mod"]

isBuildIn :: String -> Bool
isBuildIn str = elem str buildInFunctions 
