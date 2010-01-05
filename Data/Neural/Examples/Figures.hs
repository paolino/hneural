module Data.Neural.Figures where

import Data.List
import System.IO
import Control.Arrow
import Data.Maybe

type Point = (Double,Double)
type Class = Double
type Raster = Point -> Maybe Int
type Figure = [(Point,Class)]

spiral :: Figure
spiral = let 
	p i f = (4.5 * i * f i + 0.5) / 200 + 0.5 
	r = [1,1.2 .. 20.5]
	z0 = [((p i cos, p i sin), 0) | i <- r]
	z1 = [((p i (negate . cos),p i (negate . sin)), 1) | i <- r]
	in z0 ++ z1

snakeRaster :: Int -> Int -> Raster
snakeRaster nx ny (x,y) = let
	sx = floor (x * fromIntegral nx) 
	sy = floor (y * fromIntegral ny)
	in  if abs x >= 1 || abs y >= 1 then Nothing else Just (sy * nx + if even sy then sx else nx - sx - 1)

plot :: Figure -> Maybe Handle -> IO ()
plot xs h = mapM_ (hPutStrLn (maybe stdout id h) . intercalate " " . map show . (\((x,y),c) -> [x,y,c])) xs

classi :: Raster -> Figure -> Maybe Handle -> IO ()
classi r xs h = let 
	cs = map (first r) xs
	ls = catMaybes . map (\(p,q) -> p >> return (fromJust p,q)) $ cs 
	in mapM_ (hPutStrLn (maybe stdout id h) .intercalate " ". (\(n,c) -> [show n,show c])) ls


