module Parser.SchemeParser
  ( parseScheme ) where

import Data.AST(AST(..), ArithmeticArgs(..), ComparisionArgs(..), ListOperatorArgs(..), ValArgs(..))
import Data.ParseTree(ParseTree(..), Exp(..), Symbol(..), Val(..))
import Parser.BuildInFunctions(isArithmeticFunction, isComparisionFunction, isListOperator)
import Parser.RoughParser(parseRough)
import Text.Parsec(ParseError)

parseScheme :: String -> AST
parseScheme str = case parseRough str of
                    Right pt -> toAST pt
                    Left _ -> NullAST

toAST :: ParseTree -> AST
toAST (Plain exp) = createPlainAST exp 
toAST (Sharp exp) = createSharpAST exp
toAST (Quote exp) = createQuoteAST exp

createPlainAST :: Exp -> AST
createPlainAST (Exps ((Plain (SymbolExp (BuildIn s))):args)) | isArithmeticFunction s = createArithmeticAST s args
createPlainAST (Exps ((Plain (SymbolExp (BuildIn s))):args)) | isComparisionFunction s = createComparisionAST s args
createPlainAST (Exps ((Plain (SymbolExp (BuildIn s))):args)) | isListOperator s = createListOperatorAST s args
createPlainAST (Exps ((Plain (SymbolExp (Special s))):pt)) = createSpecialFormAST s pt
createPlainAST (SymbolExp (Variable v)) = ValAST $ VariableAST v
createPlainAST (ValExp (IntVal i)) = ValAST $ IntAST i
createPlainAST (ValExp (DoubleVal d)) = ValAST $ DoubleAST d

createSharpAST :: Exp -> AST
createSharpAST (ValExp (BoolVal b)) = ValAST $ BoolAST b

createQuoteAST :: Exp -> AST
createQuoteAST (Exps ps) = ListOperatorAST $ ListAST (map toAST ps)

createArithmeticAST :: String -> [ParseTree] -> AST
createArithmeticAST "+" pt = ArithmeticAST $ AdditionAST $ map toAST pt
createArithmeticAST "-" pt = ArithmeticAST $ SubtractionAST $ map toAST pt
createArithmeticAST "*" pt = ArithmeticAST $ MultiplicationAST $ map toAST pt
createArithmeticAST "/" pt = ArithmeticAST $ DivisionAST $ map toAST pt
createArithmeticAST "mod" pt = ArithmeticAST $ ModuloAST $ map toAST pt
createArithmeticAST _ _ = NullAST

createComparisionAST :: String -> [ParseTree] -> AST
createComparisionAST ">" pt = ComparisionAST $ GreaterAST $ map toAST pt
createComparisionAST "<" pt = ComparisionAST $ LessAST $ map toAST pt
createComparisionAST ">=" pt = ComparisionAST $ GreaterEqAST $ map toAST pt
createComparisionAST "<=" pt = ComparisionAST $ LessEqAST $ map toAST pt
createComparisionAST "=" pt = ComparisionAST $ EqualsAST $ map toAST pt
createComparisionAST _ _ = NullAST

createListOperatorAST :: String -> [ParseTree] -> AST
createListOperatorAST "cons" (p1:p2:[]) = ListOperatorAST $ ConsAST (toAST p1) (toAST p2)
createListOperatorAST "list" pt = ListOperatorAST $ ListAST $ map toAST pt
createListOperatorAST "car" (p:[]) = ListOperatorAST $ CarAST $ toAST p
createListOperatorAST "cdr" (p:[]) = ListOperatorAST $ CdrAST $ toAST p
createListOperatorAST "null?" (p:[]) = ListOperatorAST $ IsNullAST $ toAST p
createListOperatorAST _ _ = NullAST

createSpecialFormAST :: String -> [ParseTree] -> AST
createSpecialFormAST "if" (p1:p2:p3:[]) = IfAST (toAST p1) (toAST p2) (toAST p3)
--TODO createSpecialFormAST "cond" ((Plain (Exps pt)):[]) = CondAST $ map toAST pt
--TODO createSpecialFormAST "else" (p:[]) = ElseAST $ toAST p
--TODO createSpecialFormAST "let" (p:pt) = LetAST (toAST p) (map toAST pt)
createSpecialFormAST "define" ((Plain (Exps vals)):pt) = DefineAST (map toAST vals) (map toAST pt)
createSpecialFormAST "lambda" ((Plain (Exps vals)):pt) = LambdaAST (map toAST vals) (map toAST pt)
createSpecialFormAST "set!" (p1:p2:[]) = SetAST (toAST p1) (toAST p2)
createSpecialFormAST _ _ = NullAST
