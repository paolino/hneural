import Data.Neural.Perceptron ()
import Control.Parallel.Strategies
import System.IO
import Data.Neural.Lib
import Control.Applicative

parsePatterns :: Handle -> IO (Int,[[Double]])
parsePatterns h = do
	let p i = do
		r <- hIsEOF h
		if r then return (i,[]) else do
			ls <- map read . words <$> hGetLine h 
			(j,rs) <- rnf ls `seq` p (i + 1)
			return $ (j,ls:rs)
	p 0
			
main = do 
	(fn) <- getArgs' "args: <patterns file name>"
	h <- openFile fn ReadMode
	(i,ls) <- parsePatterns h
	print i
	hClose h		
