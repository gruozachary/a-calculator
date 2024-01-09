module RPN
    ( evaluateWith
    , evaluate
    , BinaryOp(..)
    , Token(..)
    ) where
import Data.Map (Map, lookup, empty)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Pow

data Token
    = Literal !Int
    | BinaryOp !BinaryOp
    | Variable !String

type Stack = [Int]

binaryop :: BinaryOp -> Stack -> Maybe Stack
binaryop o (x:y:xs) = Just $ (:xs) 
    $ case o of
        Add -> y + x
        Sub -> y - x
        Mul -> y * x
        Pow -> y ^ x
binaryop _ _        = Nothing

evaluate :: [Token] -> Maybe Int
evaluate = (`evaluateWith` empty)

evaluateWith :: [Token] -> Map String Int -> Maybe Int
evaluateWith ts' hm = f ts' []
    where
        f :: [Token] -> Stack -> Maybe Int
        f [] []     = Nothing
        f [] (x:_)  = Just x
        f (t:ts) xs = f ts =<< case t of
            Literal x  -> Just $ x : xs
            BinaryOp o -> binaryop o xs
            Variable i -> (:xs) <$> Data.Map.lookup i hm
