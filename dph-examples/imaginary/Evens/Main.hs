
import EvensVect
import Timing
import System.Environment
import Data.Array.Parallel.PArray	as P

-- | Command line usage information.
usage :: String
usage	= unlines ["Usage: evens <length>"]

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [len]	
	    -> run (read len)

	  _ -> putStr usage


-- | Run the benchmark.
run :: Int -> IO ()
run len
 = do	let arr	= P.fromList [0.. len - 1]

	arr `seq` return ()

	(arr', tElapsed)
		<- time 
		$  let 	arr'	= evensPA arr
		   in	arr' `seq` return arr'
					
	putStr $ prettyTime tElapsed
	putStr $ (show $ P.length arr') ++ "\n"
