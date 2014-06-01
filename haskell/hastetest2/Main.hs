module Main where

import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Haste
import Haste.Concurrent
import Haste.Graphics.Canvas

speed :: Double
speed = pi / 100

nCircles :: Int
nCircles = 7

main :: IO ()
main = do
	mCanvas <- elemById "main"
	posRef <- newMVar (0, 0)

	case mCanvas of
		Just elem -> concurrent $ do
			(Just canvas) <- getCanvas elem

			setCallback' elem OnMouseMove $ modifyMVarIO posRef . const . return . flip (,) ()
			forkIO $ runReaderT (evalStateT loop 0) (canvas, posRef)

		Nothing -> alert "Canvas?"

loop :: StateT Double (ReaderT (Canvas, MVar (Int, Int)) CIO) ()
loop = forever $ do
	angle <- get
	(canvas, posRef) <- lift ask

	io (peekMVar posRef) >>= \pos -> case pos of
		Just (x, y) -> render canvas $ drawCircles x y angle
		Nothing -> return ()

	put $ nextAngle angle
	io $ wait 20

	where
		nextAngle a = if a > pi then - pi else a + speed
		io = lift . lift

drawCircles :: Int -> Int -> Double -> Picture ()
drawCircles x y angle = mapM_ drawCircle [ 0 .. nCircles - 1 ]
	where drawCircle n = color col . fill . circle (sx, sy) $ 10
		where
			col = RGB 0 0 $ 155 + 100 * n * n `quot` nCircles

			sx = cos relativeAngle * dist + fx
			sy = sin relativeAngle * dist + fy

			relativeAngle = angle + 2 * pi * fn / (fromIntegral nCircles)
			dist = 35

			fx = fromIntegral x
			fy = fromIntegral y
			fn = fromIntegral n
