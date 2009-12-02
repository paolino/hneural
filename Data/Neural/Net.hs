{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ScopedTypeVariables #-}

-- | 

module Data.Neural.Net  where

import Data.Graph
import Data.Array.Diff 
import Control.Arrow
import Data.List
import Data.Maybe
import Control.Applicative

type Query a = Vertex -> Maybe a

data Perceptron e d = Perceptron {
	valuate :: Query d -> (d, Perceptron e d),
	correct :: (Query d, Query e) -> (Query e, Perceptron e d),
	addLink :: Direction Vertex Vertex -> (Direction d (Query e), Perceptron e d)-- ,
	}

data Net e d = Net { 
		graph :: Graph,
		filters :: (Vertex -> d -> d, Vertex -> d -> e -> e),
		perceptrons :: DiffArray Vertex (Perceptron e d),  
		queries :: DiffArray Vertex (Maybe d, Query e) 
		}

instance Show (Net e d) where
	show (Net gf fs ps qs) = show (bounds ps,bounds qs)

data Direction a b = Forward a | Backward b 

directedLike (x,_) (Forward _) = Forward x
directedLike (_,x) (Backward _) = Backward x


-- sostituzione
x /-/ y = x // [y]
-- modifica
x /!/ (y,f) = x /-/ (y, f $ x ! y)

-- sostituzione query d
setqd qs (v,qd) = qs /!/ (v,first $ const qd)

-- sostituzione query e
setqe qs (v,qe) = qs /!/ (v,second $ const qe)

operateVertex :: Direction Vertex Vertex -> Net e d -> Net e d
operateVertex (Forward v) (Net gf (fo,fi) ps qs ) = let 
	(qdv,p') = valuate (ps ! v) (fmap (fo v) . fst . (qs !))
	in Net gf (fo,fi) (ps /-/ (v,p')) $ setqd qs (v,Just qdv)
operateVertex (Backward v) (Net gf (fo,fi) ps qs) = let
	(qev,p') = correct (ps ! v) (
		\v' -> fmap (fo v') . fst $ qs ! v',
		\v' ->  fi v <$> (fst $ qs ! v) <*> (snd $ qs ! v') v
		)
	in Net gf (fo,fi) (ps /-/ (v,p')) $ setqe qs (v,qev) 

operateVertices :: Direction [Vertex] [Vertex] -> Net e d -> Net e d
operateVertices d net = foldr operateVertex net $ mapDirection d where
	mapDirection (Forward xs) = map Forward xs
	mapDirection (Backward xs) = map Backward xs

operateNet 	:: Direction [(Vertex,Maybe d)] [(Vertex,Query e)] -- ^ ingressi
		-> [Vertex] 	-- ^ uscite
		-> Net e d  	-- ^ rete di partenza
		-> Net e d  	-- ^ rete di arrivo
operateNet (Forward xs) ys (Net gf fs  ps qs)   = 
	operateVertices (Forward (containing (topSort gf) $ ys ++ map fst xs)) . Net gf fs ps $  foldl setqd qs xs
operateNet (Backward xs) ys (Net gf fs  ps qs) = 
	operateVertices (Backward (containing (topSort $ transposeG gf) $ ys ++ map fst xs)) . Net gf fs ps $ foldl setqe qs xs

containing ys xs = consume xs (dropWhile (not . flip elem xs) ys) [] where
	consume [] ys zs = zs
	consume xs [] _ = error "consume: not a subset"
	consume xs (y:ys) zs = y:consume (delete y xs) ys zs

addEdge :: Edge -> Net e d -> Net e d
addEdge e@(v0,v1) (Net gf fs ps qs) = Net 
		(buildG (bounds gf) $ e: edges gf) 
		fs  (ps // [(v0,p0),(v1,p1)]) (setqe (setqd qs (v0,Just qd)) (v1,qe))
	where	(Forward qd,p0) = addLink (ps ! v0) (Forward v1)
		(Backward qe,p1) = addLink (ps ! v1) (Backward v0)

enlarge :: DiffArray Vertex a -> a -> DiffArray Vertex a
enlarge x y = listArray (shiftBound x) $ elems x ++ [y]

shiftBound :: IArray a e => a Vertex e -> (Vertex, Vertex)
shiftBound = second (+1) . bounds 

addPerceptron :: [Vertex] -> [Vertex] -> Perceptron e d -> Net e d -> (Vertex,Net e d)
addPerceptron ins outs p (Net gf fs ps qs) = (v,foldr addEdge net es)
	where 	d@(_,v) = shiftBound gf
		es = map (flip (,) v) ins ++ map ((,) v) outs
		gf' = buildG d $ edges gf
		net = Net gf' fs (enlarge ps p) (enlarge qs (Nothing,const Nothing))

newBoard :: (Vertex  -> d -> d, Vertex -> d -> e -> e) -> Perceptron e d -> Net e d
newBoard fs p0 = Net (buildG (0,0) []) fs (listArray (0,0) [p0]) (listArray (0,0) [])

-------------------------------- Perceptrons --------------------------------------------------





