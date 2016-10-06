module Data.AST
  ( AST(..)
  , ArithmeticArgs(..)
  , ValArgs(..)
  ) where

data AST =
    NullAST
  | ListAST [AST]
  | ArithmeticAST ArithmeticArgs
  | ValAST ValArgs
      deriving Show

data ArithmeticArgs =
    AdditionAST [AST]
  | SubtractionAST [AST]
  | MultiplicationAST [AST]
  | DivisionAST [AST]
  | ModuloAST [AST]
      deriving Show

data ValArgs =
    IntAST Int
  | DoubleAST Double
  | CharVal Char
  | StringAST String
  | BoolAST Bool
      deriving Show
