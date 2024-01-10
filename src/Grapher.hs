{-# LANGUAGE TupleSections #-}
module Grapher
    ( drawGraph
    , PlotSettings(..)
    ) where
import SDL (Renderer, rendererDrawColor, V4 (V4), ($=), clear, present, pollEvents, Point(P), V2(V2), drawLine)
import RPN (Equation, evaluateWith)
import Data.Map (fromList)
import Foreign.C (CInt)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Function (on)


data PlotSettings = PlotSettings
    { dimensions :: !(Int, Int)
    , equation   :: !Equation
    , scale      :: !(Double, Double)
    }

type Plot = ReaderT PlotSettings IO

getCoordinate :: Equation -> Double -> Maybe (Double, Double)
getCoordinate e x = (x,)
    <$> evaluateWith e (fromList [("x", x)])

processCoordinate :: (Double, Double) -> Plot (Point V2 CInt)
processCoordinate c = do
    let (x, y) = c

    (sx, sy) <- asks scale

    return $ P $ V2
        (round $ 400 + x * sx)
        (round $ 300 - y * sy)

handleEvents :: Plot ()
handleEvents = void pollEvents

drawAxis :: Renderer -> Plot ()
drawAxis r = do
    (x, y) <- asks $ uncurry ((,) `on` fromIntegral) . dimensions 
    rendererDrawColor r $= V4 0 0 0 0
    drawLine r (P $ V2 0 (y `div` 2)) (P $ V2 x (y `div` 2))
    drawLine r (P $ V2 (x `div` 2) 0) (P $ V2 (x `div` 2) y)

clearScreen :: Renderer -> Plot ()
clearScreen r = do
    rendererDrawColor r $= V4 255 255 255 255
    clear r

drawGraph' :: Renderer -> Plot ()
drawGraph' r = do
    handleEvents
    clearScreen r
    drawAxis r

    rendererDrawColor r $= V4 255 0 0 255
    e <- asks equation
    (sx, _) <- asks scale
    case mapM (getCoordinate e . (/sx)) [-1000..1000] of
        Just cs' -> do
            cs <- mapM processCoordinate cs'
            mapM_ (uncurry $ drawLine r) (zip cs $ tail cs)
            present r
            drawGraph' r
        Nothing -> return ()

drawGraph :: Renderer -> PlotSettings -> IO ()
drawGraph r = runReaderT (drawGraph' r)
