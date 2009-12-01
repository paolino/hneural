{-# LANGUAGE ViewPatterns, NoMonomorphismRestriction, ScopedTypeVariables #-}

-- | 

module Data.Neural.Topology where

import Data.Graph
import Data.Array.Diff 
import Control.Arrow
import Data.List
import Data.Maybe

type Query a = Vertex -> Maybe a

data Perceptron e d = Perceptron {
	forward :: Query d -> (Query d, Perceptron e d),
	backward :: Query e -> (Query e, Perceptron e d),
	addLink :: Direction Vertex Vertex -> (Direction (Query d) (Query e), Perceptron e d)-- ,
--	removeLink :: Direction Vertex Vertex -> (Direction (Query d) (Query e), Perceptron e d)
	}

data Net e d = Net { 
		perceptrons :: DiffArray Vertex (Perceptron e d),  
		queries :: DiffArray Vertex (Query d, Query e)  
		}
instance Show (Net e d) where
	show (Net ps qs) = show (bounds ps,bounds qs)

data Direction a b = Forward a | Backward b 


directedLike (x,_) (Forward _) = Forward x
directedLike (_,x) (Backward _) = Backward x

x /-/ y = x // [y]
x /!/ (y,f) = x /-/ (y, f $ x ! y)

setqd qs (v,qd) = qs /!/ (v,first $ const qd)
setqe qs (v,qe) = qs /!/ (v,second $ const qe)

operateVertex :: Direction Vertex Vertex -> Net e d -> Net e d
operateVertex (Forward v) (Net ps qs) = let 
	(qdv,p') = forward (ps ! v) (($v) . fst . (qs !))
	in Net (ps /-/ (v,p')) $ setqd qs (v,qdv)
operateVertex (Backward v) (Net ps qs) = let
	(qev,p') = backward (ps ! v) (($v) . snd . (qs !))
	in Net (ps /-/ (v,p')) $ setqe qs (v,qev) 


operateVertices :: Direction [Vertex] [Vertex] -> Net e d -> Net e d
operateVertices d net = foldr operateVertex net $ mapDirection d where
	mapDirection (Forward xs) = map Forward xs
	mapDirection (Backward xs) = map Backward xs

operateNet 	:: Direction [(Vertex,Query d)] [(Vertex,Query e)] -- ^ ingressi
		-> [Vertex] 	-- ^ uscite
		-> Graph	-- ^ grafo dei collegamenti
		-> Net e d  	-- ^ rete di partenza
		-> Net e d  	-- ^ rete di arrivo
operateNet (Forward xs) ys g (Net ps qs)   = 
	operateVertices (Forward (containing (topSort g) $ ys ++ map fst xs)) . Net ps $  foldl setqd qs xs
operateNet (Backward xs) ys g (Net ps qs) = 
	operateVertices (Backward (containing (topSort $ transposeG g) $ ys ++ map fst xs)) . Net ps $ foldl setqe qs xs

containing ys xs = consume xs (dropWhile (not . flip elem xs) ys) [] where
	consume [] ys zs = zs
	consume xs [] _ = error "consume: not a subset"
	consume xs (y:ys) zs = y:consume (delete y xs) ys zs

addEdge :: Edge -> (Graph,Net e d) -> (Graph,Net e d)
addEdge e@(v0,v1) (g, Net ps qs) = (
		buildG (bounds g) $ e: edges g, 
		Net (ps // [(v0,p0),(v1,p1)]) (setqe (setqd qs (v0,qd)) (v1,qe))
		) 
	where	(Forward qd,p0) = addLink (ps ! v0) (Forward v1)
		(Backward qe,p1) = addLink (ps ! v1) (Backward v0)

enlarge :: DiffArray Vertex a -> a -> DiffArray Vertex a
enlarge x y = listArray (shiftBound x) $ elems x ++ [y]

shiftBound :: IArray a e => a Vertex e -> (Vertex, Vertex)
shiftBound = second (+1) . bounds 

addPerceptron :: [Vertex] -> [Vertex] -> Perceptron e d -> (Graph,Net e d) -> (Vertex,(Graph, Net e d))
addPerceptron ins outs p (g,Net ps qs) = (v,foldr addEdge (buildG d $ edges g , net) es)
	where 	d@(_,v) = shiftBound g
		es = map (flip (,) v) ins ++ map ((,) v) outs
		net = Net (enlarge ps p) (enlarge qs (const Nothing,const Nothing))

newBoard p0 = (buildG (0,0) [],Net (listArray (0,0) [p0]) (listArray (0,0) []))

-------------------------------- Perceptrons --------------------------------------------------
dumb = let 
	t@(r,p) = (const Nothing, dumb)
	in Perceptron (const t) (const t) (\z -> ((r,r) `directedLike` z,p))


scalar = (sum .) . zipWith (*)
scaled x f = map (*f) x

data Linear = Linear {
	weights :: [Double],
	indici :: ([Vertex],[Vertex]),
	output :: Double,
	errore :: Double
	}
linear 	:: Double -- ^ learning factor
	-> (Double -> Double, Double -> Double)
	-> Perceptron (Double,Double) Double
linear eta (sig,desig) = let 
	
	p l = Perceptron (f l) (g l) (n l)
	
	rqe l@(Linear ws _ _ e) = (\i -> Just $ (e, ws !! (i+1)), p l)
	rqd l@(Linear _ _ z _) = (\_ -> Just z, p l)

	f l@(Linear ws (is,_) _ _) q = rqd l{output = sig $ ws `scalar` (1 : map (fromJust . q) is) }

	g l@(Linear ws (_,os) z _ ) q = rqe l{ weights = ws `scaled` (eta * e), errore  = e }
		where 	e = (desig z *) . sum . map deq $ os
			deq x = let (e,w) = fromJust . q $ x in e * w

	n l@(Linear _ ios z _ ) (Forward v)  = first Forward . rqd $ l{ indici = second (++ [v]) ios }
	n l@(Linear ws ios _ _) (Backward v) = 
		first Backward . rqe $ l{ weights = ws ++ [0], indici = first (++ [v]) ios }
	in p $ Linear [0] ([],[]) 0 0 
