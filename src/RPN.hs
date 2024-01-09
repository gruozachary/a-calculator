module RPN
    ( evaluate
    , BinaryOp(..)
    , Token(..)
    ) where

data BinaryOp
    = Add
    | Sub
    | Mul
    | Pow

data Token
    = Literal !Int
    | BinaryOp !BinaryOp

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
evaluate = (`f` [])
    where
        f :: [Token] -> Stack -> Maybe Int
        f [] []     = Nothing
        f [] (x:_)  = Just x
        f (t:ts) xs = f ts =<< case t of
            Literal x  -> Just $ x : xs
            BinaryOp o -> binaryop o xs

