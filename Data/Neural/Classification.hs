{-# LANGUAGE ExistentialQuantification#-}

module Data.Neural.Classification where

import Data.Binary
import Data.Neural.Lib
import qualified Data.Neural.Perceptron as P
import qualified Data.Neural.Reservoir as R


type Query p c = Maybe (Cached p c)

type Domained p c = [(p,c)]

data Classification p c = forall s. Binary s => Classification (Query p c) (Domained p c -> Classification p c) s

data Trainer p c = Trainer (Query p c -> Maybe (Domained p c, Trainer p c))

train :: Trainer p c -> Classification p c -> Classification p c
train (Trainer ftr) cl@(Classification pc fcl _) = case ftr pc of 
		Just (d,tr') -> train tr' $ fcl d
		Nothing -> cl 

save :: Classification p c -> FilePath -> IO ()
save (Classification _ _ s) n = encodeFile n s

type State = (R.Reservoir, [P.Weight])

cachedR :: R.Reservoir -> Cached R.Pattern R.Class
cachedR 
classification :: State -> Classification R.Pattern R.Class

