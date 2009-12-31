{-# LANGUAGE  ScopedTypeVariables #-}
import Control.Monad (foldM_)
import Control.Applicative ((<$>))
import System.IO (stderr,stdout,hPutStr,hPutStrLn)
import Text.Printf (printf)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (hPutStr)
import System.SimpleArgs (getArgs)
import Control.Exception (catch, ErrorCall (..),evaluate)
import Prelude hiding (catch)
import System.Exit (exitSuccess, exitFailure)

import Data.Neural.Reservoir (Pattern, normalize, feed, converge)
-- | compute a -normalized on the "next" hyperspace (3D here)- list of patterns.  
patterns2D 	:: Int 			-- ^ number of samples in X 
		-> Int 			-- ^ number of samples in Y
		-> (Int,[Pattern])	-- ^ (next dimension which is 3, the list of patterns)
patterns2D sx sy = (3,[fst . normalize 1 $ [f sx x,f sy y,3] | x <- [0 .. sx - 1], y <- [0 .. sy - 1]])
	where f s x = fromIntegral x / fromIntegral s

-- | experimentally convergence constraint for normalizing vector 
eigen = 0.85
-- | fixed convergence epsilon
delta = 0.01

help =	"***\nUsage:\n\tEsnAlg <number of neurons> <connectivity> <number of samples in x>" ++
	" <number of samples in y>\n\tOutput goes to console, you should redirect it to a file\n***\n"
getArgs' = catch (getArgs >>= evaluate) $ \(ErrorCall _) -> hPutStrLn stderr help >> exitFailure

main = do
	(n,c,sizex::Int,sizey::Int) <- getArgs'
	let (ins,ps) = patterns2D sizex sizey
	ff <- feed n c ins eigen
	let te ms (i,p) = do
		h <- converge delta [0..n -1] <$> ff p ms
		hPutStrLn stdout . show .  fst . normalize 1 $ map h [0..n-1]
		hPutStr stderr $ printf "%05d\r" (i :: Int)
		return $ Just h
	foldM_ te Nothing $ zip [1..] ps  
	exitSuccess
	
