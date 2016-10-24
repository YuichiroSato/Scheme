module Data.AST
  ( AST(..)
  , ArithmeticArgs(..)
  , ComparisionArgs(..)
  , LogicArgs(..)
  , ListOperatorArgs(..)
  , PairAST(..)
  , ValArgs(..)
  ) where

data AST =
    NullAST
  | ArithmeticAST ArithmeticArgs
  | ComparisionAST ComparisionArgs
  | LogicAST LogicArgs
  | ListOperatorAST ListOperatorArgs
  | ValAST ValArgs
  | IfAST AST AST AST
  | CondAST [PairAST] (Maybe AST)
  | ElseAST AST
  | LetAST [PairAST] [AST]
  | DefineAST [AST] [AST]
  | LambdaAST [AST] [AST]
  | SetAST AST AST
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

data LogicArgs =
    AndAST [AST]
  | OrAST [AST]
  | NotAST AST
  | EqAST [AST]
     deriving Show

data ListOperatorArgs =
    ConsAST AST AST
  | ListAST [AST]
  | CarAST AST
  | CdrAST AST
  | IsNullAST AST
      deriving Show

data PairAST = PairAST AST AST deriving Show

data ValArgs =
    IntAST Int
  | DoubleAST Double
  | CharVal Char
  | StringAST String
  | BoolAST Bool
  | VariableAST String
      deriving Show
