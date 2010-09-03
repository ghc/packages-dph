
import Util
import Timing
import System.Environment
import System.Random
import Control.Exception
import qualified MergeSort			as V
import qualified LegacyList.OddEven 		as L
import qualified Data.Array.Parallel.PArray	as P
import qualified Data.Array.Parallel.Unlifted	as U

main 
 = do	args	<- getArgs
	case args of
	 [count] -> run (read count)
	 []	 -> putStr "usage: mergesort <count>\n"


run :: Int -> IO ()
run count
 | not $ isPowerOfTwo count
 = error "mergesort: length of array must be a power of two."

 | otherwise
 = do	let gen		= mkStdGen 12345
	let parr	=  P.fromUArrPA' 
			$  U.randomRs count (0, 1) gen

	evaluate $ P.nf parr

	let elems 	= (take count $ randomRs (0, 100) gen) :: [Double]
	let sorted	= L.sort elems
	
	(parr_sorted, tElapsed)
		<- time
		$  let	parr'	= V.sortCorePA parr
		   in	parr' `seq` return parr'
		
	putStr $ prettyTime tElapsed
	print  $ P.toList parr_sorted

