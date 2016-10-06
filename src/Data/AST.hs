module Data.AST
  ( AST(..)
  , ArithmeticArgs(..)
  , ComparisionArgs(..)
  , ValArgs(..)
  ) where

data AST =
    NullAST
  | ListAST [AST]
  | ArithmeticAST ArithmeticArgs
  | ComparisionAST ComparisionArgs
  | ValAST ValArgs
      deriving Show

data ArithmeticArgs =
    AdditionAST [AST]
  | SubtractionAST [AST]
  | MultiplicationAST [AST]
  | DivisionAST [AST]
  | ModuloAST [AST]
      deriving Show

data ComparisionArgs =
    GreaterAST [AST]
  | LessAST [AST]
  | GreaterEqAST [AST]
  | LessEqAST [AST]
  | EqualsAST [AST]
      deriving Show

data ValArgs =
    IntAST Int
  | DoubleAST Double
  | CharVal Char
  | StringAST String
  | BoolAST Bool
      deriving Show
