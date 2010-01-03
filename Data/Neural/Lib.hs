module Data.Neural.Lib where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Random
import Control.Monad
import System.SimpleArgs
import System.IO
import System.Exit
import Data.List (unfoldr)
import Control.Exception
import Prelude hiding (catch)


getArgs' help = catch (getArgs >>= evaluate) $ \(ErrorCall _) -> hPutStrLn stderr help >> exitFailure
-- | library: normalize the sum of the squares of a vector to k and give back the computed norm
normalize :: (Floating a) => a -> [a] -> ([a],a)
normalize k vs = (map ((*k).(/p)) vs,p) where p = sqrt . sum $ zipWith (*) vs vs

-- | library: compute a random normalized vector
vector :: (MonadRandom m , Functor m, Floating a, Random a) => a -> Int -> m [a]
vector k c = fst . normalize k <$> (replicateM c $ getRandomR (-1,1))

unspace :: String -> [String]
unspace = let cond = (== ' ') in unfoldr (Just . second (dropWhile cond). break cond)
