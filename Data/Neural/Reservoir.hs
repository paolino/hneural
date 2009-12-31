-- | This module exports useful bindings and types for building an echo state network reservoir, and find its stable states for given patterns

module Data.Neural.Reservoir (feed, converge, Pattern , Component, Signal, vector, normalize) where

import Control.Monad (replicateM, ap)
import Control.Applicative ((<$>))
import Data.Array.Unboxed as AU ((!),listArray,UArray)
import Control.Monad.Random (MonadRandom, getRandomR, Random)
import Control.Parallel (pseq) 
import Data.Maybe (isJust)
import Data.List (find)

-- | library: normalize the sum of the squares of a vector to k and give back the computed norm
normalize :: (Floating a) => a -> [a] -> ([a],a)
normalize k vs = (map ((*k).(/p)) vs,p) where p = sqrt . sum $ zipWith (*) vs vs

-- | library: compute a random normalized vector
vector :: (MonadRandom m , Functor m, Floating a, Random a) => a -> Int -> m [a]
vector k c = fst . normalize k <$> (replicateM c $ getRandomR (-1,1))

-- | neuron names
type Key = Int 
-- | the signal component type
type Component = Double
-- | a signal hiding its implementation
type Signal = Key -> Component
-- | a scalar from the signal , both synapses and neurons are scalar
type Scalar = Signal -> Component
-- | socket is a set of synapses
type Socket = [Scalar]
-- | build a socket by a list of names and a list of vector
socket :: [Key] -> [Double] -> Socket
socket = zipWith (\k w -> (w *) . ($k))
-- | sum the accesses of a socket which is an unbiased linear neuron
neuron :: Socket -> Scalar
neuron ts s = sum $ map ($s) ts 
-- | given a list of nurons evolve a signal, either from a concrete signal made of its component or by an abstract one
-- Having an abstract one is possible by picking it from a previous evolution. No check is done on the lengths of the 
-- given components
evolve :: [Scalar] -> Either [Component] Signal -> [Signal]
evolve ns x  = f ns $ either signal id x where
	f ns' = iterate $ \s -> s 0 `pseq` signal (map ($s) ns') 
	signal :: [Component] -> Signal -- make a signal from its components
	signal xs = (AU.!) $ (AU.listArray (0, length ns - 1) xs :: AU.UArray Key Double)
-- | a pattern is the list of Components of the input neurons
type Pattern = [Component]

-- | a feed is the monadic function that creates the evolution of the signal from a given pattern, 
-- it can take a previoulsly computed signal, or it will boot a new one from the monadic environment
type Feed m = Pattern -> Maybe Signal -> m [Signal]

-- | compute a feed from echo state network parameters. Weights and routes are booted from the monadic environment 
feed :: (Functor m , MonadRandom m) 
	=> Int 		-- ^ Number of neurons
	-> Int 		-- ^ Number of incoming synapses per neuron
	-> Int 		-- ^ Number of input neurons
	-> Double 	-- ^ Normalization factor (max eigenvalue)
	-> m (Feed m)	-- ^ computed Feed 
feed n c ins k = do 
		b <- replicateM n $ do 
			ks <- replicateM c $ getRandomR (0, n - 1)
			ws <- vector k $ c + ins
			return . neuron $ socket (ks ++ [n .. n + ins - 1]) ws
		return $ \is -> fmap (evolve $ b ++ map const is) . 
			maybe (Left <$> vector 1 (n + ins)) (return . Right) 
-- | evolve the Signal until a convergence is reached. Here convergence is funcion f which states that all the 
-- absolute differences between this and previous signal components are less than a given epsilon
converge 	:: Double	-- ^ epsilon 
		-> [Int] 	-- ^ indices of tested components
		-> [Signal] 	-- ^ signal evolution
		-> Signal	-- ^ converged signal
converge l js = snd . head . dropWhile f . ap zip tail  where
	f (s1,s2) = isJust . find (>l)  . map (\i -> abs (s1 i - s2 i)) $ js


