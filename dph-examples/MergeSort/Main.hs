
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
 = do	let gen		=  mkStdGen 12345
	let arrElems	=  (P.fromUArrPA' $ U.randomRs count (0, 1) gen)  :: P.PArray Double
	evaluate $ P.nf arrElems

	let gen2	=  mkStdGen 54321
	let arrElems2	=  (P.fromUArrPA' $ U.randomRs count (0, 1) gen2) :: P.PArray Double
	evaluate $ P.nf arrElems2
	
	let listElems	= P.toList arrElems
	let listElems2	= P.toList arrElems2

	let listSorted	= L.sort listElems
	let listSorted2	= L.sort listElems2
	
	(arrSorted, tElapsed)
		<- time
		$  let	parr'	= V.sortCorePA arrElems 
		   in	parr' `seq` return parr'
		
	putStr	$ prettyTime tElapsed
	print	$ P.toList arrSorted
	print	$ isSorted $ P.toList arrSorted
