module Data.AST
  ( AST(..)
  , ArithmeticArgs(..)
  , ComparisionArgs(..)
  , ListOperatorArgs(..)
  , ValArgs(..)
  ) where

data AST =
    NullAST
  | ArithmeticAST ArithmeticArgs
  | ComparisionAST ComparisionArgs
  | ListOperatorAST ListOperatorArgs
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

data ListOperatorArgs =
    ConsAST AST AST
  | ListAST [AST]
  | CarAST AST
  | CdrAST AST
  | IsNullAST AST
      deriving Show

data ValArgs =
    IntAST Int
  | DoubleAST Double
  | CharVal Char
  | StringAST String
  | BoolAST Bool
  | VariableAST String
      deriving Show
