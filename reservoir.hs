import Control.Applicative ((<$>))
import System.IO (stderr,hPutStr,hPutStrLn,hGetLine,hIsEOF,stdin,stdout,hSetBinaryMode)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.Neural.Reservoir (feed, converge,Signal, Feed)
import Text.Printf (printf)
import Data.Neural.Lib (getArgs', unspace)
import Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as BS (hPutStr,hGetContents)
import Control.Monad (foldM)
import Data.Maybe (listToMaybe)

delta = 0.01
help :: String
help =	"***\nUsage:\n\treservoir <number of neurons> <connectivity> <number of inputs> \n" ++
	"\t<max eigenvalue> <convergence factor>\n" ++
	"\tInput patterns are read from stdin\n" ++
	"\tOutput goes to console, you should redirect it to a file\n***\n" 

main :: IO b
main = do
	(b,n,c,ins,eigen) <- getArgs' help
	let 	rn = [0 .. n - 1]
	 	conv = converge delta rn
		res h = map h rn
	ff <- feed n c ins eigen
	if b == "B" then binary conv ff res
		else human conv ff res ins
	exitSuccess

human :: ([Signal] -> Signal) -> Feed IO -> (Signal -> [Double]) -> Int -> IO ()
human conv ff get ins = do
	let te (i,jh) = do
		e <- hIsEOF stdin
		if e then return () else do
			p <- map read . take ins . unspace <$> hGetLine stdin	
			h <- conv <$> ff p jh
			hPutStr stderr $ printf "%05d\r" i
			putStrLn . intercalate " " . map show . get $ h
			te $ (i + 1, Just h)	
	te (1 :: Int,Nothing)
	hPutStrLn stderr ""

binary :: ([Signal] -> Signal) -> Feed IO -> (Signal -> [Double]) -> IO ()
binary conv ff get = do
	hSetBinaryMode stdin True
	ps <- decode <$> BS.hGetContents stdin
	let te mh p = do
		h <- conv <$> ff p (listToMaybe mh)
		h 0 `seq` return (h:mh)
	map get <$> foldM te [] ps >>= BS.hPutStr stdout . encode 
	
		 
