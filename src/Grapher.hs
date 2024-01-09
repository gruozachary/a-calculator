module Grapher
    ( drawGraph
    ) where
import SDL (Renderer, rendererDrawColor, V4 (V4), ($=), clear, present, pollEvents, drawPoint, Point(P), V2(V2), drawLine)
import RPN (Equation, evaluateWith)
import Data.Map (fromList)
import Foreign.C (CInt)

getCoordinate :: Equation -> Double -> Maybe (Point V2 CInt)
getCoordinate e x =
    let y' = evaluateWith e (fromList [("x", x)])
    in case y' of
        Just y  -> Just $ P $ V2 (round (x+400)) (round (300 - y))
        Nothing -> Nothing

drawGraph :: Renderer -> Equation -> IO ()
drawGraph r e = do
    _ <- pollEvents

    rendererDrawColor r $= V4 255 255 255 255
    clear r

    rendererDrawColor r $= V4 0 0 255 255

    let cs' = mapM (getCoordinate e) [-100..100]

    case cs' of
        Just cs -> mapM_ (uncurry (drawLine r)) (zip cs (tail cs))
        Nothing -> return ()

    present r

    drawGraph r e
