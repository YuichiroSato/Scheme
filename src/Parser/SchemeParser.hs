module Parser.SchemeParser
  ( parseScheme ) where

import Data.AST(AST(..), ArithmeticArgs(..), ComparisionArgs(..), ValArgs(..))
import Data.ParseTree(ParseTree(..), Exp(..), Symbol(..), Val(..))
import Parser.BuildInFunctions(isArithmeticFunction, isComparisionFunction)
import Parser.RoughParser(parseRough)
import Text.Parsec(ParseError)

parseScheme :: String -> AST
parseScheme str = case parseRough str of
                    Right pt -> toAST pt
                    Left _ -> NullAST

toAST :: ParseTree -> AST
toAST (Plain exp) = createPlainAST exp 
toAST (Quote exp) = createQuoteAST exp

createPlainAST :: Exp -> AST
createPlainAST (Exps ((Plain (SymbolExp (BuildIn s))):args)) | isArithmeticFunction s = createArithmeticAST s args
createPlainAST (Exps ((Plain (SymbolExp (BuildIn s))):args)) | isComparisionFunction s = createComparisionAST s args
createPlainAST (ValExp (IntVal i)) = ValAST $ IntAST i
createPlainAST (ValExp (DoubleVal d)) = ValAST $ DoubleAST d

createQuoteAST :: Exp -> AST
createQuoteAST (Exps ps) = ListAST (map toAST ps)

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
