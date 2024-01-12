{-# LANGUAGE TupleSections #-}
module Grapher
    ( drawGraph
    , PlotEnvironment(..)
    , PlotState(..)
    ) where
import qualified SDL
import RPN (Equation, evaluateWith)
import qualified Data.Map (fromList)
import Data.Vector.Storable (fromList)
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

type Plot = ReaderT PlotEnvironment (StateT PlotState IO)

getCoordinate :: Equation -> Double -> Maybe (Double, Double)
getCoordinate e x = (x,)
    <$> evaluateWith e (Data.Map.fromList [("x", x)])

processCoordinate :: (Double, Double) -> Plot (SDL.Point SDL.V2 CInt)
processCoordinate (x, y) = do
    (sx, sy) <- lift $ gets scale
    (dx, dy) <- asks dimensions
    return $ SDL.P $ SDL.V2
        (round $ (fromIntegral dx / 2) + x * sx)
        (round $ (fromIntegral dy / 2) - y * sy)

getPoints :: Plot (Maybe [SDL.Point SDL.V2 CInt])
getPoints = do
    e  <- asks equation
    sx <- gets (fst . scale)
    traverse (mapM processCoordinate)
        (mapM (getCoordinate e . (/ sx)) [-1000..1000])

handleEvents :: Plot ()
handleEvents = void SDL.pollEvents

drawAxis :: SDL.Renderer -> Plot ()
drawAxis r = do
    (x, y) <- asks $ uncurry ((,) `on` fromIntegral) . dimensions 
    SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 0
    SDL.drawLine r
        (SDL.P $ SDL.V2 0 (y `div` 2))
        (SDL.P $ SDL.V2 x (y `div` 2))
    SDL.drawLine r
        (SDL.P $ SDL.V2 (x `div` 2) 0)
        (SDL.P $ SDL.V2 (x `div` 2) y)

clearScreen :: SDL.Renderer -> Plot ()
clearScreen r = do
    SDL.rendererDrawColor r SDL.$= SDL.V4 255 255 255 255
    SDL.clear r

drawGraph' :: SDL.Renderer -> Plot ()
drawGraph' r = do
    handleEvents
    clearScreen r
    drawAxis r
    SDL.rendererDrawColor r SDL.$= SDL.V4 255 0 0 255
    getPoints >>= mapM_ (SDL.drawLines r . fromList)
    SDL.present r
    drawGraph' r

drawGraph :: SDL.Renderer -> PlotEnvironment -> PlotState -> IO ()
drawGraph r = evalStateT . runReaderT (drawGraph' r)
