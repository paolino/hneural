{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction, BangPatterns #-}

module Data.Neural.Perceptron where

import Prelude hiding (foldr, foldl,concat,mapM)
import Control.Arrow ((***),(&&&))
import Control.Monad (replicateM)
import Control.Parallel.Strategies (NFData, rnf)
import Control.Parallel (pseq)
import Data.Neural.Svg (renderSvg,circle,blue,yellow)
import Data.List (partition)
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Applicative
import Control.Monad.Random (MonadRandom, getRandomR)

-- | Un percettrone come automa. Nello stato Eval contiene una funzione che dal vettore in ingresso calcola il valore in uscita accoppiato col nuovo stato dell'automa.
data Eval e d a = Eval {evalT :: [d] -> (d,Correct e d a), get :: a , reset :: (a -> a) -> Eval e d a} 


-- | Nello stato Correct contiene una funzione che riceve, o un valore (percettrone dello stato finale, o un vettore di errori (provenienti dallo strato superiore) e produce una funzione per ottenere gli errori per i suoi eventuali speriori, e il nuovo stato del percettrone
newtype Correct e d a = Correct (Either d [e] -> (Int -> e,Eval e d a))



class NetClass c g e d a where
	forward :: [d] -> (c,[Eval e d a])  -> ([d],(g,[Correct e d a]))
	backward :: [d] -> (g, [Correct e d a]) -> [d] -> (c,[Eval e d a])

type Net c e d a = (c,[Eval e d a])
-- | Come esprimere i fatti, una lista di ingressi ed una lista di uscite
type Fact d = ([d],[d])

-- | oprazione completa di allenamento su un fatto
learn :: NetClass c g e d a => Fact d -> Net c e d a -> Net c e d a
learn (xs,ys) = backward ys . snd . forward xs

-- | le funzioni di match calcolano un valore dalla lista di coppie (valore calcolato, valore voluto)
type Match d h = [(d,d)] -> h

-- | valutazione di una funzione di match su un fatto
match :: NetClass c g e d a => Match d h -> Net c e d a -> Fact d -> h
match dh n (xs, ys) = dh (zip (fst $ forward xs n) ys)

-- | supporto per la valutazione completa della rete , richiede  che d sia riducibile a forma normale. Il fatto è arbitrario
force :: (NetClass c g e d a,NFData d) => Net c e d a -> Fact d -> ()
force n = rnf . match (map fst) n 

-- | evaluate a net against an input
value :: NetClass c g e d a => Net c e d a -> Fact d -> [d]
value net =  match (map fst) net

--
-- | high level trainer, from a set of data , to the infinite generated nets, nets are forced to be evaluated 
train :: (Net c g e d a, NFData d) => (a -> a) -> [Fact d] -> Net c e d a -> [Net c e d a]
train f cs net = (iterate kernel' $ map (map $ flip reset f) net) where
	kernel' net = force net (head cs) `pseq` foldr learn net cs 

-- | the type of a perceptron creator
type Perceptron m e d a = Either Int a -> m (Eval e d a)

-- | costruisce una rete neurale a partire da un creatore di percettroni, riceve la morfologia come lista di interi
new :: Monad m => Perceptron m e d a -> [Int] -> m (Net c e d a)
new f ns = mapM (\(m,n) -> replicateM m (f (Left n))) . zip (tail ns) $ ns

-- | restore a net given perceptrons' states
restore :: Monad M => Perceptron m e d a -> [[a]]

newtype FNE e d a = FNE (Net c e d a)
instance Foldable (FNE e d)  where
	foldMap f (FNE ls) = mconcat . map (f . get) $ concat ls 	

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

netToSvg w net cs = render w $ map (fst &&& value net) cs
	


