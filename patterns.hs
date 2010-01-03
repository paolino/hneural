import System.IO (stdout,hPutStrLn)
import System.Exit (exitSuccess)
import Data.List (intercalate)


import Data.Neural.Lib (normalize, getArgs')


-- serp f = map (\(t,x) -> if f t then x else reverse x) . zip [0..]
-- remat x y = concat .  serp odd . (if odd x then reverse else id) . transpose . serp even . take x . unfoldr (Just . splitAt y)

-- | compute a -normalized on the "next" hyperspace (3D here)- list of patterns. Points follow a snake track 
patterns2D 	:: Int 			-- ^ number of samples in X 
		-> Int 			-- ^ number of samples in Y
		-> Double		-- ^ radius of normalization
		-> [[Double]]		-- ^ the list of patterns
patterns2D sx sy k = [fst . normalize k $ [f sx x,f sy y,3] 
		| x <- [0 .. sx - 1], 
		y <- if even x then [0 .. sy - 1] else [sy - 1, sy - 2.. 0]]
	where 	f s x =  fromIntegral x /fromIntegral s

main :: IO b
main = do
	(sx,sy,k) <- getArgs' "args: <number of x samples> <number of y samples> <normalization>"
	mapM_ (hPutStrLn stdout . intercalate " " . map show) $ patterns2D sx sy k
	exitSuccess
	
	

