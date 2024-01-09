module RPN
    ( evaluateWith
    , evaluate
    , BinaryOp(..)
    , Token(..)
    , parse
    , Equation(..)
    ) where
import Data.Map (Map, lookup, empty)
import Text.Read (readMaybe)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Pow

data Token
    = Literal !Double
    | BinaryOp !BinaryOp
    | Variable !String

type Stack = [Double]
newtype Equation = Equation [Token]

binaryop :: BinaryOp -> Stack -> Maybe Stack
binaryop o (x:y:xs) = Just $ (:xs) 
    $ case o of
        Add -> y + x
        Sub -> y - x
        Mul -> y * x
        Pow -> y ** x
binaryop _ _        = Nothing

evaluate :: Equation -> Maybe Double
evaluate = (`evaluateWith` empty)

evaluateWith :: Equation -> Map String Double -> Maybe Double
evaluateWith (Equation ts') hm = f ts' []
    where
        f :: [Token] -> Stack -> Maybe Double
        f [] []     = Nothing
        f [] (x:_)  = Just x
        f (t:ts) xs = f ts =<< case t of
            Literal x  -> Just $ x : xs
            BinaryOp o -> binaryop o xs
            Variable i -> (:xs) <$> Data.Map.lookup i hm

parseLexeme :: String -> Maybe Token
parseLexeme xs = Just $ case xs of
    "+" -> BinaryOp Add
    "-" -> BinaryOp Sub
    "*" -> BinaryOp Mul
    "^" -> BinaryOp Pow
    _   ->
        case readMaybe xs of
            Just n  -> Literal n
            Nothing -> Variable xs

parse :: String -> Maybe Equation
parse = fmap Equation . mapM parseLexeme . words
