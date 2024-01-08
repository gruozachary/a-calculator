module RPN
    ( evaluate
    , Token(..)
    ) where

data Token
    = Literal !Int
    | Add
    | Sub
    | Mult

type Stack = [Int]

add :: Stack -> Maybe Stack
add (x:y:xs) = Just (x + y : xs)
add _        = Nothing

sub :: Stack -> Maybe Stack
sub (x:y:xs) = Just (x + y : xs)
sub _        = Nothing

mult :: Stack -> Maybe Stack
mult (x:y:xs) = Just (x * y : xs)
mult _ = Nothing

evaluate :: [Token] -> Maybe Int
evaluate = (`f` [])
    where
        f ::  [Token] -> Stack -> Maybe Int
        f [] []       = Nothing
        f [] (x:_)    = Just x
        f (t:ts) xs = f ts =<< case t of
            Literal x -> Just $ x : xs
            Add       -> add xs
            Sub       -> sub xs
            Mult      -> mult xs

