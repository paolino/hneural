-- | This module exports useful bindings and types for building an echo state network reservoir, and find its stable states for given patterns

module Data.Neural.Reservoir (feed, converge, Pattern , reservoir, Reservoir, 
	Component, Signal, vector, normalize, Feed) where

import Control.Monad (replicateM, ap, forM)
import Control.Applicative ((<$>))
import Data.Array.Unboxed as AU ((!),listArray,UArray)
import Control.Monad.Random (MonadRandom, getRandomRs, getRandomR, Random)
import Control.Parallel (pseq) 
import Data.Maybe (isJust)
import Data.List (nub, find)

import Data.Neural.Lib (vector, normalize)

-- | neuron names
type Key = Int 
-- | the signal component type
type Component = Double
-- | a signal hiding its implementation
type Signal = Key -> Component
-- | a scalar from the signal , both synapses and neurons are scalar
type Scalar = Signal -> Component
-- | socket is a set of synapses
-- | sum the accesses of a socket which is an unbiased linear neuron
neuron :: [(Key,Double)] -> Scalar
neuron ts s = sin . sum .  map (\(k,w) -> w * s k) $ ts

-- | given a list of nurons evolve a signal, either from a concrete signal made of its component or by an abstract one
-- Having an abstract one is possible by picking it from a previous evolution. No check is done on the lengths of the 
-- given components
evolve :: [Scalar] -> Either [Component] Signal -> [Signal]
evolve ns = tail . iterate (\s -> signal (map ($s) ns)) . either signal id  where
	signal :: [Component] -> Signal -- make a signal from its components
	signal xs = (AU.!) $ (AU.listArray (0, length ns - 1) xs :: AU.UArray Key Double)
-- | a pattern is the list of Components of the input neurons
type Pattern = [Component]

-- | a feed is the monadic function that creates the evolution of the signal from a given pattern, 
-- it can take a previoulsly computed signal, or it will boot a new one from the monadic environment
type Feed m = Pattern -> Maybe Signal -> m [Signal]

-- | The serialization of a reservoir. This is a sparse matrix compressed rendering
type Reservoir = [[(Key,Component)]]

-- | build a random reservoir , given some parameters
reservoir :: (Functor m , MonadRandom m) 
	=> Int 		-- ^ Number of neurons
	-> Int 		-- ^ Number of incoming synapses per neuron
	-> Int 		-- ^ Number of input neurons
	-> Double 	-- ^ Normalization factor (max eigenvalue)
	-> m Reservoir	-- ^ Reservoir
reservoir n c ins k = do 
		replicateM n $ do 
			ks <- take c <$> getRandomRs (0, n - 1)
			ws <- vector k $ c + ins
			return $ zip (ks ++ [n .. n + ins - 1]) ws

-- | compute a feed from a reservoir. Parameters must be given to boot a dimensionally valid signal 
feed :: (Functor m , MonadRandom m) 
	=> Reservoir	-- ^ given Reservoir
	-> Int		-- ^ inputs
	-> Int		-- ^ neurons
	-> Feed m	-- ^ computed Feed 
feed r ins n is = fmap (evolve $ map neuron r ++ map const is) . 
		maybe (Left <$> vector 1 (n + ins)) (return . Right) 

-- | evolve the Signal until a convergence is reached. Here convergence is funcion f which states that all the 
-- absolute differences between this and previous signal components are less than a given epsilon
converge 	:: Double	-- ^ epsilon 
		-> [Int] 	-- ^ indices of tested components
		-> [Signal] 	-- ^ signal evolution
		-> Signal	-- ^ converged signal
converge l js = snd . head . dropWhile f . ap zip tail  where
	f (s1,s2) = isJust . find (>l)  . map (\i -> abs (s1 i - s2 i)) $ js


