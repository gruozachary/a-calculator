{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RPN (parse)
import Grapher (drawGraph)
import SDL (initializeAll, createWindow, defaultWindow, createRenderer, defaultRenderer)

main :: IO ()
main = do
    initializeAll
    w <- createWindow "Graph" defaultWindow
    r <- createRenderer w (-1) defaultRenderer

    putStrLn "Enter a valid RPN expression: "
    e' <- parse <$> getLine

    case e' of
        Just e -> drawGraph r e
        Nothing -> putStrLn "Invalid expression"
