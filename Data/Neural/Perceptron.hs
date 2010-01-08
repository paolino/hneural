
{-# LANGUAGE ViewPatterns#-}

-- | A perceptron is a linear classifier . It can corrrect its weights to respect a given set of pattern->class relations
module Data.Neural.Perceptron where

import Data.Neural.Signal
import Data.Neural.Classification

deltaRule 	:: [(Int,Double)] -- ^ Signal component indices, with their boot weight
		-> Double 	-- ^ boot bias
		-> Double 	-- ^ learning factor
		-> Cortex
deltaRule iws b l = cortex ws where
	(is, (b:) -> ws) = unzip iws
	vs s = 1:map s is
	c ws = sum . zipWith (*) ws . vs
	t ws (s,y') = let 
		y = c ws s
		e = (y' - y) * l
		in cortex $ zipWith (*) ws (map (*e) . vs $ s)
	cortex ws = Cortex (c ws) (t ws)
