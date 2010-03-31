
-- | Reading and writing matricies as ASCII files.
--	We use ASCII so we can generate and check simple test data by hand,
--	and we don't want to fool around with byte order issues.
--
--   Matrix file format is like:
--
--	MATRIX			-- header
--	100 100			-- width and height
--	1.23 1.56 1.23 ...	-- data, separated by whitespace
--	....
--
-- TODO: Merge this with PPM.hs
--
module Matrix
	( readMatrixFromTextFile
	, genRandomMatrix)
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.List				as L
import Array					as A
import Prelude					as P
import System.IO
import Control.Monad
import Data.Char
import Debug.Trace
import System.Random


-- Reading ----------------------------------------------------------------------------------------
readMatrixFromTextFile
	:: (U.Elt a, Num a, Read a)
	=> FilePath			-- ^ File name of matrix file.
	-> IO (Array DIM2 a)	

readMatrixFromTextFile fileName
 = do	handle		<- openFile fileName ReadMode
	
	"MATRIX"	<- hGetLine handle
	[width, height]	<- liftM (P.map read . words) $ hGetLine handle
	str		<- hGetContents handle
	let vals	= readValues str

	let dim		= () :. width :. height
	let mat		= toArray dim $ U.fromList vals

	return mat

	
-- | Read a string containing ints separated by whitespace.	
readValues :: (Num a, Read a) => String -> [a]
readValues cs	= readValues' [] cs
 where	readValues' _ []	= []
	readValues' acc (c : rest)
		| isSpace c
		= if null acc 
			then readValues' [] rest
			else read (reverse acc) : readValues' [] rest

		| isDigit c
		= readValues' (c : acc) rest

		| otherwise
		= error $ "unexpected char in Matrix file " ++ show (ord c)


-- Random -----------------------------------------------------------------------------------------
genRandomMatrix 
	:: (Random a, U.Elt a, Num a)
	=> DIM2 
	-> IO (Array DIM2 a)

genRandomMatrix sh
 = do	stdGen	<- getStdGen
	let arr	= toArray sh
			$ U.fromList 
			$ take (A.size sh)
			$ randomRs (0, 1) stdGen
		
	return arr


