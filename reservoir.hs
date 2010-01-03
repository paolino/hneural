import Control.Applicative ((<$>))
import System.IO (stderr,hPutStr,hPutStrLn,hGetLine,hIsEOF,stdin)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.Neural.Reservoir (feed, converge)
import Text.Printf (printf)
import Data.Neural.Lib (getArgs', unspace)


help :: String
help =	"***\nUsage:\n\treservoir <number of neurons> <connectivity> <number of inputs> \n" ++
	"\t<max eigenvalue> <convergence factor>\n" ++
	"\tInput patterns are read from stdin\n" ++
	"\tOutput goes to console, you should redirect it to a file\n***\n" 

main :: IO b
main = do
	(n,c,ins,eigen,delta) <- getArgs' help
	let rn = [0 .. n - 1]
	ff <- feed n c ins eigen
	let te (i,jh) = do
		e <- hIsEOF stdin
		if e then return () else do
			p <- map read . take ins . unspace <$> hGetLine stdin	
			h <- converge delta rn <$> ff p jh
			hPutStr stderr $ printf "%05d\r" i
			putStrLn . intercalate " " . map (show . h) $ rn
			te $ (i + 1, Just h)	
	te (1 :: Int,Nothing)
	hPutStrLn stderr ""
	exitSuccess
	
