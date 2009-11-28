{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Neural.Linear where

import Control.Arrow ((&&&))
import Control.Monad.Random (MonadRandom, getRandomR)
import Control.Monad (replicateM)
import Control.Parallel.Strategies (NFData)
import Data.Neural.Perceptron (Eval (..), Correct (..), NetEval, mkNet)


-------------------------- Linear net ---------------------------------------
newtype Linear = Linear ([Double],Double,Double) deriving (Show,NFData)
data LinearParam = LinearParam 
	{ sigmoidF :: (Double -> Double, Double -> Double), convergence :: Double, rate :: Double}
type LinearP = Eval Linear Double Double
type LinearN = NetEval Linear Double Double

-- | Compute the input valutation by multiplying weights and inputs
scalar 	:: Linear 	-- ^ weights
	-> [Double] 	-- ^ inputs
	-> Double	-- ^ output
scalar (Linear (ws,t,_)) xs = t + (sum $ zipWith (*) ws xs)

-- | Correct a perceptron weights
correct 	:: Linear	-- ^ weights 
		-> [Double] 	-- ^ inputs
		-> Double 	-- ^ delta output
		-> Double 	-- ^ annealing
		-> Linear	-- ^ new weights
correct (Linear (ws,t,a)) xs e r = Linear (zipWith (+) ws $ map (*e) xs, t + e,a - r)

-- | produce un percettrone linearo
linear 	:: LinearParam -- ^ parametri di convergenza 
		-> Linear -- ^ pesi iniziali
		-> Eval Linear Double Double -- ^ percettrone in stato valutativo

linear (LinearParam (sig,desig) _ r) = Eval . f  where
	-- avanti, dai pesi e gli ingressi si calcola l'usicta z e la si chiude in Correct
	f wt xs = (id &&& Correct . g wt xs) . sig $ wt `scalar` xs  
	-- indietro, da i pesi il valore di uscita gli ingressi e le uscite si calcolano i nuovi pesi
	-- e si chiudono nel nuovo Eval
	g wt@(Linear (_,_,alfa)) xs z ye = ((e*).(ws' !!) , Eval $ f wt') where
		e = alfa * desig z * case ye of
			Left y -> y - z  -- caso strato finale, le uscite sono valori, non errori
			Right es -> sum es -- le uscite sono errori, provenienti dallo strato superiore
		wt'@(Linear (ws',_,_)) = correct wt xs e r


-----------------------------------------------------

mkLinearN :: (Functor m, MonadRandom m) => LinearParam -> [Int] -> m LinearN
mkLinearN p@(LinearParam _ alfa _) = mkNet  (fmap (linear p) .  mkLinearP ) where
	mkLinearP n = do	(t:ws) <- replicateM (n + 1) $ getRandomR (-1,1)
				return $ Linear (ws,t,alfa)


