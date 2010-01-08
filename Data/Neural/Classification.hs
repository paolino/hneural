
module Data.Neural.Classification where

import Data.Binary
import Data.Neural.Lib
import Data.Neural.Signal
import Control.Arrow
import Control.Applicative

data Classification m = Classification 
	(Pattern -> m (Class,Classification m))
	((Pattern,Class) -> m (Classification m))

data Core m = Core (Pattern -> m (Signal, Core m))

data Cortex = Cortex (Signal -> Class) ((Signal,Class) -> Cortex)

classification :: (Monad m, Functor m) => Core m -> Cortex -> Classification m
classification r c = Classification (classify r c) (train r c) where
	classify (Core r) c@(Cortex o _) p = (o *** flip classification c) <$> r p
	train (Core r) (Cortex _ t) (p,cl) = do
		(s,r') <- r p
		return . classification r' . t $ (s,cl)
