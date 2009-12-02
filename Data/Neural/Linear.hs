{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Neural.Linear where

import Control.Arrow (first,second)
import Data.Graph (Vertex)
import Data.Maybe (fromJust)
import Data.Neural.Math (scaled,scalar)
import Data.Neural.Net

data Linear a b = Linear {
	weights :: [a],
	indici :: ([Vertex],[Vertex]),
	output :: a,
	errore :: a,
	state :: b
	}

common f g b0 = let 

	p l = Perceptron (rqd . f l) (rqe . g l) (n l)
	
	rqe l@(Linear ws _ _ e _) = (\i -> Just $ (e, ws !! (i+1)), p l)
	rqd l@(Linear _ _ z _ _) = (z, p l)

	n l@(Linear _ ios z _ _) (Forward v)  = 
		first Forward . rqd $ l{ indici = second (++ [v]) ios }
	n l@(Linear ws ios _ _ _) (Backward v) = 
		first Backward . rqe $ l{ weights = ws ++ [0], indici = first (++ [v]) ios }
	in p $ Linear [0] ([],[]) 0 0 b0 


linear :: (Num a) => Perceptron (a, a) a
linear = let
	f l@(Linear ws (is,_) _ _ _) q = l{output = ws `scalar` (1 : map (fromJust . q) is) }

	g l@(Linear ws (is,os) z _ _) (qd,qe) = l{ weights = zipWith (+) ws dws, errore  = e }
		where 	e = sum . map deq $ os
			deq x = let (e,w) = fromJust . qe $ x in e * w
			dws = (1:map (fromJust . qd) is) `scaled` e
	in common f g ()
{-
data Covariant a = { deltas :: [a] , errors :: [a], outputs :: [a], ncycle = Int}

linearCov :: (Num a) => Int -> Perceptron (a, a) a
linearCov n =  let 
	f l@(Linear ws (is,_) _ _ c) q = l{output = 0 , c{ outputs = v:outputs c}}
		where v = ws `scalar` (1 : map (fromJust . q) is) }
		
	g l@(Linear ws (_,os) z _ _ (Covariant ds es os m)) q 
		| m < n = 

 
		l{ weights = ws + xs `scaled` e, errore  = e }
		where 	e = sum . map deq $ os
			deq x = let (e,w) = fromJust . q $ x in e * w
			
	in common f g ()
-}
