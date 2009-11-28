{-# LANGUAGE ExistentialQuantification #-}

-- hacked from roberto tazzoli experiment --
module Svg  where

import Data.List (intercalate)
import System.Random (randomRIO)
import Control.Monad (liftM2, replicateM)

data EBox = forall a . Show a => E String a
data Elem = Elem {name :: String, attrs :: [EBox]} 

renderSvg :: Int -> Int -> [Elem] -> String
renderSvg w h l = intercalate "\n" $ [
	"<?xml version=\"1.0\"?>",
	concat [
		"<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"" ,
		show w,
		"\" height=\"",
		show h,
		"\">"
		]
	] ++ map renderElem l ++ ["</svg>"]
renderElem :: Elem -> String 
renderElem (Elem name attrs) = "   <" ++ name ++ renderAttrs attrs ++ "/>" 
renderAttrs = concatMap (\(E k v) -> " " ++ k ++ "=\"" ++ show v ++ "\"")



----------------------------------------------------------------------------------------------------------------
newtype Color = Color (Int, Int, Int)
instance Show Color where
	show (Color c) = "rgb" ++ show c

[black,green,silver,lime,gray,olive,white,yellow,maroon,navy,red,blue,purple,teal,fuchsia,aqua]= map Color
	[(0,0,0),(0,128,0),(192,192,192),(0,255,0),(128,128,128),(128,128,0),(255,255,255),(255,255,0),
		(128,0,0),(0,0,128),(255,0,0),(0,0,255),(128,0,128),(0,128,128),(255,0,255),(0,255,255)]
	
circle :: (Int,Int) -> Int -> Color -> Elem
circle (x,y) r color = Elem "circle" [E "cx" x,E "cy" y,E "r" r,E "fill" color] 

line :: (Int,Int) -> (Int,Int) -> Color -> Elem
line (x1,y1) (x2,y2) color = Elem "line" [E "x1" x1, E "y1" y1,E "x2" x2,E "y2" y2,E "stroke" color] 

---------------------------------- Test --------------------------------

maino = do
	ps <- replicateM 40 $ liftM2 (,) (randomRIO (0,300)) $ randomRIO (0,300)
	let 	eps = map (\p -> circle p 4 red) ps
		els = map (\(p,q) -> line p q blue) . zip ps $ tail ps
	writeFile "testSvg.svg" . renderSvg 300 300 $ eps ++ els




    

