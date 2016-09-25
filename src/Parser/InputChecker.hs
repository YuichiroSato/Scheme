module Parser.InputChecker
  ( CodeString
  , nullCode
  , appendCode
  , isValidCode
  , isExit
  , toString
  ) where

import Control.Monad.Trans.Writer.Lazy(runWriter, tell, Writer)

data CodeString = CodeString { codes :: [String], parenthesesCount :: Int, inString :: Bool } deriving Show

nullCode :: CodeString
nullCode = CodeString [] 0 False

appendCode :: CodeString -> String -> CodeString
appendCode (CodeString codes count inStr) str = CodeString newCodes newCount newInString
  where
    newCodes = codes ++ [trimed]
    ((newCount, newInString), trimed) = runWriter $ countParentheses str count inStr

isValidCode :: CodeString -> Bool
isValidCode (CodeString _ count _) = count == 0

isExit :: CodeString -> Bool
isExit (CodeString ("(exit)":_) _ _) = True
isExit _ = False

countParentheses :: String -> Int -> Bool -> Writer String (Int, Bool)
countParentheses  "" count isStr = return (count, isStr)
countParentheses str@(c:_) count isStr = do
  if c /= '(' && count == 0
    then return (0, isStr)
    else countIter str count isStr

countIter :: String -> Int -> Bool -> Writer String (Int, Bool)
countIter "" count isStr = return (count, isStr)
countIter (';':str) count False = return (count, False)
countIter ('"':str) count True  = do { tell "\""; countIter str count False }
countIter ('"':str) count False = do { tell "\""; countIter str count True }
countIter ('\\':'\"':str) count True = do { tell "\\\""; countIter str count True }
countIter (c:str) count True = do { tell [c]; countIter str count True}
countIter (c:str) count isStr = do
  tell [c]
  let m = count + par2Num c
  if m == 0
    then return (0, False)
    else countIter str m isStr

par2Num :: Char -> Int
par2Num '(' = 1
par2Num ')' = (-1)
par2Num _   = 0

toString :: CodeString -> String
toString (CodeString cs _ _) = foldl (\x y -> x ++ " " ++ y) "" cs
