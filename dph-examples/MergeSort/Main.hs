
import qualified MergeSort			as V
import qualified LegacyList.OddEven 		as L
import qualified Data.Array.Parallel.PArray	as P
import System.Environment
import System.Random

main 
 = do	args	<- getArgs
	case args of
	 [count] -> run (read count)
	 []	 -> putStr "usage: mergesort <count>\n"


run :: Int -> IO ()
run count
 = let	gen	= mkStdGen 12345
	elems 	= (take count $ randomRs (0, 100) gen) :: [Double]
	sorted	= L.sort elems
   in	do 	print elems
		print sorted
		print $ P.toList (V.sortCorePA $ P.fromList elems)

--	$ isSorted sorted && (sum elems == sum sorted)



isSorted :: [Int] -> Bool
isSorted xx
 = case xx of
	[]		-> True
	[x]		-> True
	(x1:x2:rest)
	 | x1 <= x2	-> isSorted (x2 : rest)
	 | otherwise	-> False
	
