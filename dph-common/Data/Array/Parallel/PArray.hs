-- |Underlying data types and type classes for parallel arrays.
--
--	For motivational material see:
--	   "An Approach to Fast Arrays in Haskell", Chakravarty and Keller, 2003
--
--	For discussion of how the mapping to generic types works see:
--         "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
--
-- TODO: Describe structure of PArrays.


module Data.Array.Parallel.PArray (
  PArray, PA, Random(..),

  length, empty, replicate, singleton, (!:),
  zip, unzip, enumFromTo, fromList, toList, nf, 
  fromUArrPA'
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.PArray.PReprInstances
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Scalar
import qualified Data.Array.Parallel.Unlifted as U

import Data.Array.Parallel.Base ( showsApp )

import qualified System.Random as R

import Prelude hiding ( length, replicate, zip, unzip, enumFromTo )

length :: PA a => PArray a -> Int
{-# INLINE length #-}
length = lengthPA_v

empty :: PA a => PArray a
{-# INLINE empty #-}
empty = emptyPA

replicate :: PA a => Int -> a -> PArray a
{-# INLINE replicate #-}
replicate = replicatePA_v

singleton :: PA a => a -> PArray a
{-# INLINE singleton #-}
singleton = singletonPA_v

(!:) :: PA a => PArray a -> Int -> a
{-# INLINE (!:) #-}
(!:) = indexPA_v

zip :: (PA a, PA b) => PArray a -> PArray b -> PArray (a,b)
{-# INLINE zip #-}
zip = zipPA_v

unzip :: (PA a, PA b) => PArray (a,b) -> (PArray a, PArray b)
{-# INLINE unzip #-}
unzip = unzipPA_v

enumFromTo :: Int -> Int -> PArray Int
{-# INLINE enumFromTo #-}
enumFromTo = enumFromToPA_v

fromList :: PA a => [a] -> PArray a
{-# INLINE fromList #-}
fromList = fromListPA

toList :: PA a => PArray a -> [a]
toList xs = [indexPA_v xs i | i <- [0 .. length xs - 1]]

nf :: PA a => PArray a -> ()
nf = nfPA

instance (PA a, Show a) => Show (PArray a) where
  showsPrec n xs = showsApp n "fromList<PArray>" (toList xs)

class Random a where
  randoms  :: R.RandomGen g => Int -> g -> PArray a
  randomRs :: R.RandomGen g => Int -> (a, a) -> g -> PArray a

prim_randoms :: (Scalar a, R.Random a, R.RandomGen g) => Int -> g -> PArray a
prim_randoms n = fromUArrPA' . U.randoms n

prim_randomRs :: (Scalar a, R.Random a, R.RandomGen g) => Int -> (a, a) -> g -> PArray a
prim_randomRs n r = fromUArrPA' . U.randomRs n r

instance Random Int where
  randoms = prim_randoms
  randomRs = prim_randomRs

instance Random Double where
  randoms = prim_randoms
  randomRs = prim_randomRs

