{-# LANGUAGE ScopedTypeVariables #-}

import Timing
import Vectorised
import System.IO
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Array.Parallel
import System.Environment
import qualified Data.Vector                    as V
import qualified Vector                         as V
import Control.Exception                        (evaluate)
import qualified Data.Vector.Unboxed            as U
import qualified Data.Array.Parallel.Unlifted   as P
import Data.Array.Parallel.PArray               as PA

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, reps, fileName] -> run alg (read reps) fileName
	  _	                -> usage
	
usage	
 = putStr $ unlines
	[ "usage: smvm <alg> <reps> <file>" ]


-- Vectorised Nested Data Parallel Version
run "vectorised" reps fileName
 = do	(matrix, vector)    <- loadPA fileName

	matrix `seq` return ()
	vector `seq` return ()

--	-- Multiply sparse matrix by the dense vector.
--	(vResult, tElapsed)
--	 <- time $ let result	= smvmPA 0 matrix vector
--		   in  PA.nf result `seq` return result
					
        -- Multiply sparse matrix by the dense vector
        (vResult, tElapsed)
         <- time $ let loop n
                         | n == 0    
                         = let  result   = smvmPA n matrix vector
                           in   PA.nf result `seq` result

                         | otherwise 
                         = let  result   = smvmPA n matrix vector
                           in   PA.nf result `seq` loop (n - 1)

                       final       = loop reps
                   in  final `seq` return final

	-- Print how long it took.
	putStr $ prettyTime tElapsed

	-- Print some info about the test setup.
	putStrLn $ "vector length   = " ++ show (U.length (PA.toUArray vector))
	
--        putStrLn $ "matrix          = " ++ show matrix

	-- Print checksum of resulting vector.
	putStrLn $ "result sum      = " ++ show (U.sum    (PA.toUArray vResult))


-- Sequential version using Data.Vector
run "vector" reps fileName
 = do   (segd, uaMatrix, uaVector) <- loadUArr fileName
        let vMatrix     = U.fromList $ P.toList uaMatrix
        let vVector     = U.fromList $ P.toList uaVector

        let matrix
                = V.force
                $ V.map U.force
                $ V.zipWith 
                        (\start len -> U.slice start len vMatrix)
                        (U.convert $ P.indicesSegd segd)
                        (U.convert $ P.lengthsSegd segd)


        let vector      = U.fromList $ U.toList uaVector
        matrix `seq` return ()
        vector `seq` return ()

        -- Multiply sparse matrix by the dense vector
        (vResult, tElapsed)
         <- time $ let loop n
                         | n == 0    
                         = let  result   = U.force $ V.smvm n matrix vector
                           in   result `seq` result

                         | otherwise 
                         = let  result   = U.force $ V.smvm n matrix vector
                           in   result `seq` loop (n - 1)

                       final       = loop reps

                   in  final `seq` return final
                                        
        -- Print how long it took.
        putStr $ prettyTime tElapsed

        -- Print some info about the test setup.
        putStrLn $ "vector length   = " ++ show (U.length vector)
        
--        putStrLn $ "matrix          = " ++ show matrix

        -- Print checksum of resulting vector.
        putStrLn $ "result sum      = " ++ show (U.sum   vResult)


-- Load Matrices --------------------------------------------------------------
-- | Load a test file containing a sparse matrix and dense vector.
loadPA 	:: String 				-- ^ filename.
	-> IO  ( PArray (PArray (Int, Double))	-- sparse matrix
	       , PArray Double)			-- dense vector

loadPA fileName
 = do 	(segd, arrMatrixElems, arrVector) <- loadUArr fileName

    	let paMatrix	= PA.nestUSegd segd (PA.fromUArray2 arrMatrixElems)
	let paVector	= PA.fromUArray arrVector
	return (paMatrix, paVector)


-- | Load a test file containing a sparse matrix and dense vector.
loadUArr :: String			-- ^ filename
	 -> IO ( P.Segd			-- segment descriptor saying what array elements
					--    belong to each row of the matrix.
	       , P.Array (Int, Double)	-- column indices and matrix elements
	       , P.Array Double)	-- the dense vector

loadUArr fname 
 = do	h <- openBinaryFile fname ReadMode

	-- check magic numbers at start of file to guard against word-size screwups.
	alloca $ \ptr -> do
		hGetBuf h ptr (sizeOf (undefined :: Int))
		magic1 :: Int	<- peek ptr
		hGetBuf h ptr (sizeOf (undefined :: Int))
		magic2	:: Int <- peek ptr
		if magic1 == 0xc0ffee00 Prelude.&& magic2 == 0x12345678 
			then return ()
			else error $ "bad magic in " ++ fname

	-- number of elements in each row of the matrix.
	lengths <- P.hGet h

	-- indices of all the elements.
	indices <- P.hGet h

	-- values of the matrix elements.
	values  <- P.hGet h

	-- the dense vector.
	vector  <- P.hGet h

	evaluate lengths
	evaluate indices
	evaluate values
	evaluate vector

	let segd    = P.lengthsToSegd lengths
	    matrix  = P.zip indices values

	return (segd, matrix, vector)
