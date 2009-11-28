{-# LANGUAGE ExistentialQuantification #-}

-- hacked from roberto tazzoli experiment --
module Data.Neural.Svg (Attr (..), Elem (..), renderSvg,
	black,green,silver,lime,gray,olive,white,yellow,
	maroon,navy,red,blue,purple,teal,fuchsia,aqua,
	circle, line)
	   where


import Data.List (intercalate)

-- | An attribute is made by a name and a showable value
data Attr = forall a . Show a => A String a

-- | An elem is made by a name and a list of attributes
data Elem = Elem String [Attr] 

-- | render a list of elements in a given size box. The xml is given as String
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

-- element renderer
renderElem :: Elem -> String 
renderElem (Elem name attrs) = "   <" ++ name ++ renderAttrs attrs ++ "/>" 

-- attributes renderer
renderAttrs :: [Attr] -> [Char]
renderAttrs = concatMap (\(A k v) -> " " ++ k ++ "=\"" ++ show v ++ "\"")



----------------------------------------------------------------------------------------------------------------
newtype Color = Color (Int, Int, Int)
instance Show Color where
	show (Color c) = "rgb" ++ show c

-- | possible colors
black,green,silver,lime,gray,olive,white,yellow,maroon,navy,red,blue,purple,teal,fuchsia,aqua :: Color
[black,green,silver,lime,gray,olive,white,yellow,maroon,navy,red,blue,purple,teal,fuchsia,aqua]= map Color
	[(0,0,0),(0,128,0),(192,192,192),(0,255,0),(128,128,128),(128,128,0),(255,255,255),(255,255,0),
		(128,0,0),(0,0,128),(255,0,0),(0,0,255),(128,0,128),(0,128,128),(255,0,255),(0,255,255)]

-- | a circle given coordinate , radius and color	
circle :: (Int,Int) -> Int -> Color -> Elem
circle (x,y) r color = Elem "circle" [A "cx" x,A "cy" y,A "r" r,A "fill" color] 

-- | a line given start and end point
line :: (Int,Int) -> (Int,Int) -> Color -> Elem
line (x1,y1) (x2,y2) color = Elem "line" [A "x1" x1, A "y1" y1,A "x2" x2,A "y2" y2,A "stroke" color] 






    

