{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module PrimesVect (primesPA) where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int 
import qualified Prelude

primesPA :: Int -> PArray Int
primesPA n = toPArrayP (primes n)

primes :: Int -> [:Int:]
primes n  
	| n == 1	= emptyP 
	| n == 2	= singletonP 2
	| otherwise	= sps +:+ [: i | i <- enumFromToP (sq+1) n, notMultiple sps i:] 
	where
		sps	= primes sq
 		sq	= sqrt n

		notMultiple :: [:Int:] -> Int -> Bool
		notMultiple ps i = andP [: mod i p /= 0 | p <- ps:]
