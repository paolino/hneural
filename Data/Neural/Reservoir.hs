{-# LANGUAGE ScopedTypeVariables #-}
-- | This module exports useful bindings and types for building an echo state network reservoir, and find its stable states for given patterns

module Data.Neural.Reservoir where

import Control.Monad (replicateM, ap)
import Control.Applicative ((<$>))
import Data.Array.Unboxed as AU ((!),listArray,UArray)
import Control.Monad.Random (MonadRandom, getRandomRs)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.List (find)

import Data.Neural.Lib (vector, normalize)
import Data.Neural.Signal (Signal,Sword, Key, Component,Pattern)

-- | computing a component from a signal, which is a scalar opearation
type Scalar = Signal -> Component

-- | a neuron in a reservoir. given a list of synapses , expressed as (Key,Double) == (input neuron, weight)
neuron :: [(Key,Double)] -> Scalar
neuron ts s = sin . sum .  map (\(k,w) -> w * s k) $ ts

-- | given a list of nurons evolve a signal, either from a concrete signal made of its component or by an abstract one
-- Having an abstract one is possible by picking it from a previous evolution. No check is done on the lengths of the 
-- given components
evolve :: [Scalar] -> Either [Component] Signal -> [Signal]
evolve ns = tail . iterate (\s -> s 0 `seq` signal (map ($s) ns)) . either signal id  where
	signal :: [Component] -> Signal -- make a signal from its components
	signal xs = (AU.!) $ (AU.listArray (0, length ns - 1) xs :: AU.UArray Key Double)

-- | a feed is the monadic function that creates the evolution of the signal from a given pattern, 
-- it can take a previoulsly computed signal, or it will boot a new one from the monadic environment
type Feed m = Pattern -> Maybe Signal -> m [Signal]

-- | The serialization of a reservoir. This is a sparse matrix compressed rendering
type Reservoir = (Int,[[(Key,Component)]])

-- | build a random reservoir , given some parameters
reservoir :: (Functor m , MonadRandom m) 
	=> Int 		-- ^ Number of neurons
	-> Int 		-- ^ Number of incoming synapses per neuron
	-> Int 		-- ^ Number of input neurons
	-> Double 	-- ^ Normalization factor (max eigenvalue)
	-> m Reservoir	-- ^ Reservoir
reservoir n c ins k = fmap ((,) ins) . replicateM n $ do 
			ks <- take c <$> getRandomRs (0, n - 1)
			ws <- vector k $ c + ins
			return $ zip (ks ++ [n .. n + ins - 1]) ws

-- | compute a feed of a reservoir
feed :: (Functor m , MonadRandom m) 
	=> Reservoir	-- ^ given Reservoir
	-> Feed m	-- ^ computed Feed 
feed (ins,r)  is = fmap (evolve $ map neuron r ++ map const is) . 
		maybe (Left <$> vector 1 (length r + ins)) (return . Right) 

-- | Reservoir operation, hiding cache and last signal inside.
data Operation m = Operation (Pattern -> m (Signal, Operation m))

-- | build an Operation given a Sword and a Reservoir, the funcion Pattern -> m Signal is cached, and booting Signal, for every Pattern is the result of last computation (or cache picking).
operation :: forall m . (MonadRandom m, Functor m) => Sword -> Reservoir -> Operation m
operation sw rs = Operation  $ op M.empty Nothing where
	op ca ms p = do 
		(s,ca') <- case p `M.lookup` ca of 
			Nothing -> do 	s <- sw <$> feed rs p ms 
					return (s,M.insert p s ca) 
			Just s -> return (s,ca) 
		return (s, Operation . op ca' $ Just s)

-- | build A Sword from a boolean function on last 2 signals. It returns the second in case of True	
stopBy :: ((Signal,Signal) -> Bool) -> Sword
stopBy f = snd . head . dropWhile f . ap zip tail

-- | states that all the absolute differences between this and previous signal components are less than a given epsilon
absoluteEpsilon :: Double -> [Int] -> (Signal,Signal) -> Bool
absoluteEpsilon l js (s1,s2) = isJust . find (>l)  . map (\i -> abs (s1 i - s2 i)) $ js

	
	
