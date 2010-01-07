-- | A perceptron is a linear classifier . It can corrrect its weights to respect a given set of pattern->class relations
module Data.Neural.Perceptron where

import Data.Maybe
import Control.Monad.Random
import Control.Monad
import Control.Applicative
import Data.Neural.Lib
import Data.Neural.Signal

type WFact = (Double,Fact)
type Weights = [Double]
type Error = Double

data Perceptron = Perceptron {correct :: WFact -> (Perceptron, (Error,Weights))}

runPerceptron :: Perceptron -> [WFact] -> [(Error,Weights)]
runPerceptron p = tail . map snd . scanl (correct . fst) (p,(0,[])) 

data Feed = Feed {fact :: WFact , feed :: Error -> Maybe Feed}

runFeed :: Feed -> [Error] -> [WFact]
runFeed f = tail . map fact . catMaybes . takeWhile isJust . scanl (\f w -> f >>= ($w) . feed ) (Just f)  

run :: Feed -> Perceptron -> Weights
run f p = last ws where
	(es,ws) = unzip . runPerceptron p $ fact f: fs
	fs = runFeed f es

------------------------------------------------------------------------
-------------- ZOO -------------------------------------------------------

type Activator = (Double -> Double, Double -> Double)

deltaRule :: (Functor m , MonadRandom m) => Int -> Activator -> m Perceptron	
deltaRule ins (sig,desig) = let
	perceptron = Perceptron . g  
	g ws (c,(xs,y)) = (perceptron ws', (e,ws')) where
		y' = sig . sum $ zipWith (*) ws (1:xs)
		e = y - y'
		ws' = zipWith (+) ws (map ((* desig y') . (*c) . (*e)) (1:xs))
	in perceptron <$> replicateM (ins + 1) (getRandomR (-1,1))

deltaRule'' :: (Functor m , MonadRandom m) => Int -> Activator -> Double -> m Perceptron	
deltaRule''  ins (sig,desig) k = let
	perceptron = Perceptron . g  
	g mws (c,(xs,y)) = (perceptron mws', (e,map peek mws')) where
		y' = sig . sum $ zipWith (*) ws (1:xs)
		e = y - y'
		ws' = zipWith (+) ws (map ((* desig y') . (*c) . (*e)) (1:xs))
		ws = map peek mws
		mws' = zipWith inject mws ws'
	in perceptron . map (inject (media k)) <$> replicateM (ins + 1) (getRandomR (-1,1))



dumbFeed :: [Double] -> [Fact] -> Feed
dumbFeed rs fs = feed $ concatMap (\r -> zip (repeat r) fs) rs where
	feed [x] = Feed x $ const Nothing
	feed (x:xs) = Feed x $ const (Just $ feed xs)






facts = [([0,0],0),([1,1],1)] :: [Fact]

annealing = [0.5,0.49 .. 0]


df = dumbFeed annealing facts :: Feed
dr = deltaRule'' 2 (id , const 1) 0.5 :: IO Perceptron
dr' = deltaRule 2 (sigmoid , desigmoid)  :: IO Perceptron

main = do 
	w <- run df <$> dr'
	print w
	let eval ws xs = sum $ zipWith (*) ws (1:xs)
	mapM_ (print . eval w . fst) $ facts 
