module Main (main) where

import RPN (evaluate, parse)

main :: IO ()
main = do
    putStrLn "Please enter an RPN expression:"
    x <- maybe "Invalid expression!" (show . evaluate) . parse <$> getLine
    putStrLn x
    main
