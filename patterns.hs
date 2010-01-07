import System.IO (stdout,hPutStrLn,hClose,hSetBinaryMode,hSetBuffering,BufferMode (..))
import System.Exit (exitSuccess)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as BS (hPutStr)
import Data.Binary (encode)


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
	(b,sx,sy,k) <- getArgs' "args: <number of x samples> <number of y samples> <normalization>"
	let ps = patterns2D sx sy k
	if b == "B" then do
		hSetBinaryMode stdout True
		BS.hPutStr stdout . encode $ ps
		else mapM_ (hPutStrLn stdout . intercalate " " . map show) ps
	hClose stdout
	exitSuccess
	
	

