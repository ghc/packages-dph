-- Matrix vector multiplication in Haskell (using various array
-- implementations)
--
-- NB: To be precise, we measure the computation of the vector sum of the
--     result vector of the matrix vector multiplication.
--
-- Compile and run with 
--
--   ghc -ffi -O2 -fliberate-case-threshold100 -o matvecmul MatVecMul.hs\
--     matvecmul.o && ./matvecmul +RTS -K30M

-- standard libraries
import CPUTime
import Monad
import Random

-- FFI
import Foreign
import Foreign.C

-- GHC libraries
import Data.Array
import Data.Array.IArray  (IArray)
import Data.Array.Unboxed (UArray)
import qualified
       Data.Array.Unboxed as U
import Data.Array.MArray  (newArray_, unsafeFreeze, writeArray)
import Data.Array.ST	  (STUArray)
import Control.Monad.ST	  (ST, runST)
import Control.Exception  (evaluate)
import System.Mem	  (performGC)

import Data.Array.Base (unsafeAt)
import GHC.Arr (unsafeIndex)


-- arrays types
--
type Vector  = Array  Int        Float
type Matrix  = Array  (Int, Int) Float
type UVector = UArray Int        Float
type UMatrix = UArray (Int, Int) Float
type CVector = Ptr Float
type CMatrix = Ptr Float


-- generates a random vector of the given length in NF
--
generateVector :: Int -> IO Vector
generateVector n =
  do
    rg <- newStdGen
    let fs  = take n $ randomRs (-100, 100) rg
	arr = listArray (0, n - 1) fs
    evaluate $ sum (elems arr)    -- make sure it is brought in NF
    return arr

-- generates a random square matrix in NF
--
generateMatrix :: Int -> IO Matrix
generateMatrix n =
  do
    rg <- newStdGen
    let fs  = take (n * n) $ randomRs (-100, 100) rg
	arr = listArray ((0, 0), (n - 1, n - 1)) fs
    evaluate $ sum (elems arr)    -- make sure it is brought in NF
    return arr

-- convert a standard Haskell array into an unboxed array in NF
--
arrayToIArray :: (Ix i, IArray arr e, Num e) => Array i e  -> IO (arr i e)
arrayToIArray a = 
  do
    let ia = U.listArray (bounds a) . elems $ a
    evaluate $ sum (U.elems ia)
    return ia

-- convert a vector into a CVector in NF
--
arrayToCArray :: (Ix i, Storable e) => Array i e  -> IO (Ptr e)
arrayToCArray a = newArray (elems a)

-- compute the dot product 
--

-- vanilla
mvm1 :: Matrix -> Vector -> IO (Vector, Float)
{-# NOINLINE mvm1 #-}
mvm1 a v = do
	     let (n, m) = snd (bounds a)
	         r = listArray (0, n) 
			       [sum [a!(i,j) * v!j| j <- [0..m]]
			       | i <- [0..n]]
	     s <- evaluate $ sum (elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- explicit inner loop
mvm3 :: Matrix -> Vector -> IO (Vector, Float)
{-# NOINLINE mvm3 #-}
mvm3 a v = do
	     let (n, m) = snd (bounds a)
	         r = listArray (0, n) [loop i 0 | i <- [0..n]]
		     where
		       loop i j | j > m     = 0
				| otherwise = a!(i,j) * v!j + loop i (j + 1)
	     s <- evaluate $ sum (elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- explicit inner loop w/ acc
mvm4 :: Matrix -> Vector -> IO (Vector, Float)
{-# NOINLINE mvm4 #-}
mvm4 a v = do
	     let (n, m) = snd (bounds a)
	         r = listArray (0, n) [loop i 0 0 | i <- [0..n]]
		     where
		       loop i j acc 
		         | j > m     = acc
			 | otherwise = loop i (j + 1) (acc + a!(i,j) * v!j)
	     s <- evaluate $ sum (elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- vanilla
umvm1 :: UMatrix -> UVector -> IO (UVector, Float)
{-# NOINLINE umvm1 #-}
umvm1 a v = do
	     let (n, m) = snd (U.bounds a)
	         r = U.listArray (0, n) 
			       [sum [a U.!(i,j) * v U.!j | j <- [0..m]]
			       | i <- [0..n]]
	     s <- evaluate $ sum (U.elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- explicit inner loop
umvm3 :: UMatrix -> UVector -> IO (UVector, Float)
{-# NOINLINE umvm3 #-}
umvm3 a v = do
	     let (n, m) = snd (U.bounds a)
	         r = U.listArray (0, n) [loop i 0 | i <- [0..n]]
		     where
		       loop i j | j > m     = 0
				| otherwise = a U.!(i,j) * v U.!j + 
					      loop i (j + 1)
	     s <- evaluate $ sum (U.elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- explicit inner loop w/ acc
umvm4a :: UMatrix -> UVector -> IO (UVector, Float)
{-# NOINLINE umvm4a #-}
umvm4a a v = do
	     let (n, m) = snd (U.bounds a)
	         r = U.listArray (0, n) [loop i 0 0 | i <- [0..n]]
		     where
		       loop i j acc 
		         | j > m     = acc
			 | otherwise = loop i (j + 1) 
					    (acc + a U.!(i,j) * v U.!j)
	     s <- evaluate $ sum (U.elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- explicit inner loop w/ acc forcing inlining
umvm4b :: UMatrix -> UVector -> IO (UVector, Float)
{-# NOINLINE umvm4b #-}
umvm4b a v = do
	     let (n, m) = snd (U.bounds a)
	         r = U.listArray (0, n) [loop i 0 0 | i <- [0..n]]
		     where
		       loop i j acc 
		         | j > m     = acc
			 | otherwise = loop i (j + 1) 
					    (acc + a !!!(i,j) * v !!!j)
	     s <- evaluate $ sum (U.elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- ST monad for array creation
umvm5 :: UMatrix -> UVector -> IO (UVector, Float)
{-# NOINLINE umvm5 #-}
umvm5 a v = do
	     let (n, m) = snd (U.bounds a)
	         r = runST (do
		       ma <- newArray_ (0, n)
		       outerLoop ma 0
		       unsafeFreeze ma
		     )
		     where
		       outerLoop :: STUArray s Int Float -> Int -> ST s ()
		       outerLoop ma i 
		         | i > n     = return ()
			 | otherwise = do
				         writeArray ma i (loop i 0 0)
					 outerLoop ma (i + 1)
		       loop i j acc 
		         | j > m     = acc
			 | otherwise = loop i (j + 1) 
--					    (acc + a U.!(i,j) * v U.!j)
					    (acc + a !!!(i,j) * v !!!j)
	     s <- evaluate $ sum (U.elems r)
	     return (r, s)  -- returning both guarantees that the sum can't be
			    -- fused into the main computations

-- Forcing the inlining of indexing
(!!!) :: (IArray a e, Ix i) => a i e -> i -> e
{-# INLINE (!!!) #-}
arr !!! i | (l,u) <- U.bounds arr = unsafeAt arr (unsafeIndex (l,u) i)
--arr !!! i | (l,u) <- U.bounds arr = unsafeAt arr (index (l,u) i)
  where
    index b i | U.inRange b i = unsafeIndex b i
	      | otherwise   = error "Error in array index"


-- merciless C code
foreign import ccall "matvecmul.h" 
  cmvm :: CMatrix -> CVector -> Int -> IO Float
  -- returns sum only as the C compiler won't fuse the sum in to the loop 
  -- anyway

-- execute a function and print the result and execution time
--
execAndTime :: String	       -- description
	    -> IO Float        -- benchmarked computation
	    -> IO ()
execAndTime desc comp =
  do
    putStrLn $ "\n*** " ++ desc
    performGC
    start  <- getCPUTime
    result <- comp
    end    <- getCPUTime
    let duration = (end - start) `div` 1000000000
    putStrLn $ "Result sum  : " ++ show result
    putStrLn $ "Running time: " ++ show duration ++ "ms"

main :: IO ()
main  = do
  putStrLn "Matrix vector multiplication benchmark"
  putStrLn "======================================"
  putStrLn $ "[time resolution: " ++ show (cpuTimePrecision `div` 1000000000)++
	     "ms]"
  --
  m <- generateMatrix 100
  v <- generateVector 100
  execAndTime "H98 arrays (compr) [n = 100]" (liftM snd $ mvm1 m v)
  --
  m <- generateMatrix 200
  v <- generateVector 200
  execAndTime "H98 arrays (compr) [n = 200]" (liftM snd $ mvm1 m v)
  execAndTime "H98 arrays (explicit inner loop) [n = 200]" 
    (liftM snd $ mvm3 m v)
  execAndTime "H98 arrays (explicit inner loop w/ acc) [n = 200]" 
    (liftM snd $ mvm4 m v)
  --
  m <- generateMatrix 400
  v <- generateVector 400
  execAndTime "H98 arrays (compr) [n = 400]" (liftM snd $ mvm1 m v)
  execAndTime "H98 arrays (explicit inner loop) [n = 400]" 
    (liftM snd $ mvm3 m v)
  execAndTime "H98 arrays (explicit inner loop w/ acc) [n = 400]" 
    (liftM snd $ mvm4 m v)
  um <- arrayToIArray m
  uv <- arrayToIArray v
  execAndTime "UArray (compr) [n = 400]" (liftM snd $ umvm1 um uv)
  execAndTime "UArray (explicit inner loop) [n = 400]" 
    (liftM snd $ umvm3 um uv)
  execAndTime "UArray (explicit inner loop w/ acc) [n = 400]" 
    (liftM snd $ umvm4a um uv)
  execAndTime "UArray (explicit inner loop w/ acc & inlining) [n = 400]" 
    (liftM snd $ umvm4b um uv)
  execAndTime "UArray (ST monad and loop) [n = 400]" 
    (liftM snd $ umvm5 um uv)
  --
  m <- generateMatrix 800
  v <- generateVector 800
  execAndTime "H98 arrays (compr) [n = 800]" (liftM snd $ mvm1 m v)
  execAndTime "H98 arrays (explicit inner loop) [n = 800]" 
    (liftM snd $ mvm3 m v)
  execAndTime "H98 arrays (explicit inner loop w/ acc) [n = 800]" 
    (liftM snd $ mvm4 m v)
  um <- arrayToIArray m
  uv <- arrayToIArray v
  execAndTime "UArray (compr) [n = 800]" (liftM snd $ umvm1 um uv)
  execAndTime "UArray (explicit inner loop) [n = 800]" 
    (liftM snd $ umvm3 um uv)
  execAndTime "UArray (explicit inner loop w/ acc) [n = 800]" 
    (liftM snd $ umvm4a um uv)
  execAndTime "UArray (explicit inner loop w/ acc & inlining) [n = 800]" 
    (liftM snd $ umvm4b um uv)
  execAndTime "UArray (ST monad and loop) [n = 800]" 
    (liftM snd $ umvm5 um uv)
  cm <- arrayToCArray m
  cv <- arrayToCArray v
  execAndTime "C [n = 800]" (cmvm cm cv 800)
  --
  m <- generateMatrix 1000
  v <- generateVector 1000
  cm <- arrayToCArray m
  cv <- arrayToCArray v
  execAndTime "C [n = 1000]" (cmvm cm cv 1000)
