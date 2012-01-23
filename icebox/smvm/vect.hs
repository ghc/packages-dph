{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}

import SMVMVect (smvm)

import Control.Exception (evaluate)
import System.IO

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.PArray as P

import Bench.Benchmark
import Bench.Options
import Foreign.Storable
import Foreign.Marshal.Alloc

main = ndpMain "Sparse matrix/vector multiplication (vectorised)"
               "[OPTION] ... FILES ..."
               run [] ()

run opts () files
  = do
      benchmark opts (uncurry smvm)
                (map loadSM files)
                (\arr -> nf arr `seq` ()) showRes
      return ()
  where
    showRes arr = "sum = " ++ show (U.sum (toUArrPA arr))


loadSM :: String -> IO (Point (PArray (PArray (Int, Double)), PArray Double))
loadSM s 
 = do pnt <- loadSM' s
      return $ fmap (\(segd, m, v) -> (nestUSegdPA' segd (fromUArrPA_2' m), fromUArrPA' v)) pnt


loadSM' :: String -> IO (Point (U.Segd, U.Array (Int, Double), U.Array Double))
loadSM' fname 
 = do
    h <- openBinaryFile fname ReadMode

    -- check magic numbers at start of file to guard against word-size screwups.
    alloca $ \ptr -> do
	hGetBuf h ptr (sizeOf (undefined :: Int))
	(magic1 :: Int)	<- peek ptr
	hGetBuf h ptr (sizeOf (undefined :: Int))
	(magic2	:: Int) <- peek ptr
	if magic1 == 0xc0ffee00 && magic2 == 0x12345678 
		then return ()
		else error $ "bad magic in " ++ fname

    -- number of elements in each row of the matrix.
    lengths <- U.hGet h

    -- indices of all the elements.
    indices <- U.hGet h

    -- values of the matrix elements.
    values  <- U.hGet h

    -- the dense vector.
    vector  <- U.hGet h

    evaluate lengths
    evaluate indices
    evaluate values
    evaluate vector

    putStrLn $ "sum of matrix elements = " ++ show (U.sum values)
    putStrLn $ "sum of vector elements = " ++ show (U.sum vector)

    let segd    = U.lengthsToSegd lengths
        matrix  = U.zip indices values

    return $ mkPoint (  "cols =" ++ show (U.length vector)  ++ ", "
                     ++ "rows =" ++ show (U.length lengths) ++ ", "
                     ++ "elems=" ++ show (U.length matrix))
             (segd, matrix, vector)
