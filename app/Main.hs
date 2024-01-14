{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RPN (parse)
import Grapher (drawGraph, PlotEnvironment(..), PlotState(..))
import SDL (initializeAll, createWindow, defaultWindow, createRenderer, defaultRenderer, Renderer, clear, present, rendererDrawColor, ($=), V4(V4))

appLoop :: Renderer -> IO ()
appLoop r = do
    rendererDrawColor r $= V4 255 255 255 255
    clear r
    present r

    putStrLn "Enter a valid RPN expression: "
    e' <- parse <$> getLine

    case e' of
        Just e -> do
            drawGraph r
                (PlotEnvironment
                    { equation   = e
                    , dimensions = (800, 600)
                    })
                (PlotState
                    { scale      = (100,100)
                    })
        Nothing -> putStrLn "Invalid expression"
    appLoop r

main :: IO ()
main = do
    initializeAll
    w <- createWindow "Graph" defaultWindow
    r <- createRenderer w (-1) defaultRenderer

    appLoop r
