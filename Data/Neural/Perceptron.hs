{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction, BangPatterns #-}

module Data.Neural.Perceptron where

import Control.Arrow ((***))
import Control.Monad (replicateM)
import Control.Parallel.Strategies (NFData, rnf)
import Svg (renderSvg,circle,blue,yellow)
import Data.List (partition)
import Control.Monad.Random (MonadRandom, getRandomR)

-- | Un percettrone come automa. Nello stato Eval contiene una funzione che dal vettore in ingresso calcola il valore in uscita accoppiato col nuovo stato dell'automa.
newtype Eval a e d = Eval { evalT :: [d] -> (d,Correct a e d)} 
-- | Nello stato Correct contiene una funzione che riceve, o un valore (percettrone dello stato finale, o un vettore di errori (provenienti dallo strato superiore) e produce una funzione per ottenere gli errori per i suoi eventuali speriori, e il nuovo stato del percettrone
newtype Correct a e d = Correct (Either d [e] -> (Int -> e,Eval a e d))
	
type NetEval a e d = [[Eval a e d]]
type NetCorrect a e d = [[Correct a e d]]

-- | Valuta gli strati di percettroni  producendo i valori di uscita dell'ultimo strato e la nuova rete con i percettroni in stato Correct 
forward :: [d] -> NetEval a e d -> ([d], NetCorrect a e d) 
forward xs = ((++xs) *** reverse) . foldl k (xs,[]) 
	where 	k (xs',ls) = (id *** (:ls)) . unzip . map (($xs') . evalT)

-- | Percorre la rete al contrario, passando al nuovo stato Eval attraverso la correzione
backward  :: [d] -> NetCorrect a e d -> NetEval a e d
backward  ys = snd . foldr (\l (ys',ls) -> (Right *** (:ls)) . unzip . k l $ ys' ) (Left ys,[]) 
	where	k l (Right fys)  = map (\(i,Correct f) -> f . Right . map ($i) $ fys) $ zip [0..] l 
		k l (Left ys') = map (\(y, Correct f) -> f $ Left y) $ zip ys' l

-- | Come esprimere i fatti, una lista di ingressi ed una lista di uscite
type Fact d = ([d],[d])

-- | oprazione completa di allenamento su un fatto
learn :: Fact d -> NetEval a e d -> NetEval a e d
learn (xs,ys) = backward ys . snd . forward xs

-- | le funzioni di match calcolano un valore dalla lista di coppie (valore calcolato, valore voluto)
type Match d h = [(d,d)] -> h

-- | valutazione di una funzione di match su un fatto
match :: Match d h -> NetEval a e d -> Fact d -> h
match dh n (xs, ys) = dh (zip (fst $ forward xs n) ys)

-- | supporto per la valutazione completa della rete , richiede  che d sia riducibile a forma normale. Il fatto è arbitrario
force :: NFData d => NetEval a e d -> Fact d -> ()
force n = rnf . match (map fst) n 

-- | costruisce una rete neurale a partire da un creatore di percettroni, riceve la morfologia come lista di interi
mkNet :: Monad m => (Int -> m (Eval a e d)) -> [Int] -> m (NetEval a e d)
mkNet f ns = mapM (\(m,n) -> replicateM m (f n)) . zip (tail ns) $ ns

-----------------------------------------------------------------
-- | fatto random creati attraverso un classificatore	
randomFact 	:: MonadRandom m 
		=> Int			-- ^ dimensione ingressi 
		-> [[Double] -> Bool]	-- ^ classificatore
		-> m (Fact Double)	-- ^ 
randomFact n fs	 = do
	xs <- replicateM n $ getRandomR (0,1) 
	return $ (xs, map (\f -> if f xs then 1 else 0) fs)

-- | attivatori classici
gmoids :: (Double -> Double, Double -> Double)
gmoids = (\s -> 1/(1 + exp (-s)),\z -> z * (1- z))


----------------- SVG stuff -----------------------------------------------

-- | write an Svg report to file
render :: String -> [Fact Double] -> IO ()
render fn xs = writeFile fn . renderSvg 400 400 $ map (point blue) ps ++ map (point yellow) ns where
	(ps,ns) = (map k *** map k) . partition (\(_,x:_) -> x > 0.5) $ xs 
	k =  map (floor  . (+50) . (*300)). fst
	point c (x:y:_) = circle (x,y) 2 c
	point _ _ = error "point of too less dims"
	


