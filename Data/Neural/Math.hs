{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Neural.Math where

-- | scalar product between vectors
scalar :: (Num a) => [a] -> [a] -> a
scalar = (sum .) . zipWith (*)

-- | product by scalar
scaled :: (Num a) => [a] -> a -> [a]
scaled x f = map (*f) x

