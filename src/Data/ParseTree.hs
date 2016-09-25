module Data.ParseTree 
  ( ParseTree(..)
  , Exp(..)
  , Symbol(..)
  , Val(..)
  ) where

data ParseTree =
    EmptyTree
  | Plain Exp
  | Sharp Exp
  | Quote Exp
  | BackQuote Exp
    deriving Show
data Exp =
    ValExp Val
  | SymbolExp Symbol
  | Exps [ParseTree]
    deriving Show
data Symbol =
    Special String
  | BuildIn String
  | Variable String
    deriving Show
data Val =
    EmptyList
  | IntVal Int
  | DoubleVal Double
  | CharVal Char
  | StringVal String
  | BoolVal Bool
    deriving Show
