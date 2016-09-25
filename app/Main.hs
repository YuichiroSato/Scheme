module Main where

import Parser.InputChecker(CodeString, nullCode, appendCode, isValidCode, isExit, toString)
import Parser.SchemeParser(parseScheme)

main :: IO ()
main = do
  putStrLn "hello scheme"
  startLoop

startLoop :: IO ()
startLoop = mainLoop nullCode

mainLoop :: CodeString -> IO ()
mainLoop code = do
  line <- getLine
  let newCode = appendCode code line
  if isValidCode newCode
    then evalCode newCode
    else mainLoop newCode

evalCode :: CodeString -> IO ()
evalCode code = do
  if isExit code
    then return ()
    else do
      putStrLn "start eval"
      putStrLn $ show $ parseScheme $ toString $ code
      startLoop
