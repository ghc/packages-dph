
import SumSquaresVect
import Timing
import Randomish
import System.Environment
import qualified Data.Vector.Unboxed	as V

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, len] 	-> run alg (read len) 
	  _		-> usage
	
run alg num
 = do	(result, tElapsed) <- runAlg alg num

	putStr	$ prettyTime tElapsed
	print result

runAlg "dph" num
 =	time	$ let result	= sumSq num
		  in  result `seq` return result

runAlg "vector" num
 =	time	$ let result	= V.sum 
				$ V.map (\x -> x * x) 
				$ V.map fromIntegral 
				$ V.enumFromTo 1 num

		  in  result `seq` return result
		
usage
 = putStr $ unlines
 	[ "usage: sumsq <alg> <length>"
 	, "  alg one of " ++ show ["dph", "vector"] ]
