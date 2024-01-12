{-# LANGUAGE TupleSections #-}
module Grapher
    ( drawGraph
    , PlotEnvironment(..)
    , PlotState(..)
    ) where
import SDL (Renderer, rendererDrawColor, V4 (V4), ($=), clear, present, pollEvents, Point(P), V2(V2), drawLine)
import RPN (Equation, evaluateWith)
import Data.Map (fromList)
import Foreign.C (CInt)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, asks, runReaderT, lift)
import Control.Monad.State (StateT, gets, evalStateT)
import Data.Function (on)


data PlotEnvironment = PlotEnvironment
    { dimensions :: !(Int, Int)
    , equation   :: !Equation
    }

newtype PlotState = PlotState
    { scale :: (Double, Double)
    }

--type Plot = ReaderT PlotSettings IO

type Plot = ReaderT PlotEnvironment (StateT PlotState IO)

pairs :: [a] -> Maybe [(a, a)]
pairs []        = Nothing
pairs xs@(_:ys) = Just (zip xs ys)

getCoordinate :: Equation -> Double -> Maybe (Double, Double)
getCoordinate e x = (x,)
    <$> evaluateWith e (fromList [("x", x)])

processCoordinate :: (Double, Double) -> Plot (Point V2 CInt)
processCoordinate (x, y) = do
    (sx, sy) <- lift $ gets scale
    (dx, dy) <- asks dimensions
    return $ P $ V2
        (round $ (fromIntegral dx / 2) + x * sx)
        (round $ (fromIntegral dy / 2) - y * sy)

getPoints :: Plot (Maybe [Point V2 CInt])
getPoints = do
    e  <- asks equation
    sx <- gets (fst . scale)
    traverse (mapM processCoordinate)
        (mapM (getCoordinate e . (/ sx)) [-1000..1000])

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

drawLines :: Renderer -> [Point V2 CInt] -> Plot ()
drawLines r = mapM_ (mapM (uncurry $ drawLine r)) . pairs

drawGraph' :: Renderer -> Plot ()
drawGraph' r = do
    handleEvents
    clearScreen r
    drawAxis r
    rendererDrawColor r $= V4 255 0 0 255
    getPoints >>= mapM_ (drawLines r)
    present r
    drawGraph' r

drawGraph :: Renderer -> PlotEnvironment -> PlotState -> IO ()
drawGraph r pe = evalStateT $ runReaderT (drawGraph' r) pe
