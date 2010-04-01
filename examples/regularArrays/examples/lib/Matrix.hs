
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
	, writeMatrixAsTextFile
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
-- | Read a matrix from a text file.
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

-- Writing ----------------------------------------------------------------------------------------
-- | Write a matrix as a text file.
writeMatrixAsTextFile 
	:: (U.Elt a, Show a)
	=> Array DIM2 a			-- ^ Matrix to write.
	-> FilePath			-- ^ File name of output file.
	-> IO ()

writeMatrixAsTextFile arr fileName
 = do	file	<- openFile fileName WriteMode	

	hPutStrLn file "MATRIX"

	let () :. width :. height	
		= arrayShape arr

	hPutStrLn file $ show width ++ " " ++ show height
	
	hWriteValues file $ U.toList $ fromArray arr
	hClose file
	

-- | Write out matrix values to a file.
hWriteValues
	:: Show a
	=> Handle 
	-> [a] 				-- ^ Data values.
	-> IO ()

hWriteValues handle xx
 = go xx
 where
	go []		= return ()
	go (x:xs)
	 = do	hPutStr handle $ show x
		hPutStr handle $ "\n"
		go xs


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


