

module Data.Neural.Lib where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Random
import Control.Monad
import System.SimpleArgs
import System.IO
import System.Exit
import Data.List
import Control.Exception
import qualified Data.Map as M
import Prelude hiding (catch)

sigmoid x = 1/(1 + exp (-x))
desigmoid z = 1 * (1 -z)
getArgs' help = catch (getArgs >>= evaluate) $ \(ErrorCall _) -> hPutStrLn stderr help >> exitFailure
-- | library: normalize the sum of the squares of a vector to k and give back the computed norm
normalize :: (Floating a) => a -> [a] -> ([a],a)
normalize k vs = (map ((*k).(/p)) vs,p) where p = sqrt . sum $ zipWith (*) vs vs

-- | library: compute a random normalized vector
vector :: (MonadRandom m , Functor m, Floating a, Random a) => a -> Int -> m [a]
vector k c = fst . normalize k <$> (replicateM c $ getRandomR (-1,1))

unspace :: String -> [String]
unspace = let cond = (== ' ') in unfoldr (Just . second (dropWhile cond). break cond)

data Online a b = Online {
	inject :: a -> Online a b,
	peek :: b
	}

onLine :: Online a b -> [a] -> [b]
onLine o = map peek . scanl inject o

media :: Double -> Online Double Double
media k = 	let f y x = Online (f y') y' 
			where y' = k * x + (1 - k) * y
		in Online (f 0) 0

deviazione :: Double -> Double -> Online Double Double
deviazione k k' =	let f y z x = Online (f y' z') (sqrt $ peek z'- peek y' ^ 2) where
				z' = inject z $ x ^ 2
				y' = inject y x
			in Online (f (media k) (media k)) 0

data Cached a b = Cached {eval :: a -> (Cached a b, b)}
cached f = let 
	g a x = maybe (q a x) (const (cached' a) &&& id ) (x `M.lookup` a) 
	q a x = (cached' (M.insert x y a), y ) where
		y = f x
	cached' a = Cached $ g a
	in Cached $ g M.empty  

mapCached :: Cached a b -> [a] -> (Cached a b,[b])
mapCached = mapAccumL eval 
