module Data.Array.Parallel.PArray (
  PArray, Elt,

  length, empty, replicate, singleton,
  zip, unzip, enumFromTo, fromList, nf
) where

import Data.Array.Parallel.Lifted.PArray ( PArray, PA, emptyPA, fromListPA, nfPA )
import Data.Array.Parallel.Lifted.Instances
import Data.Array.Parallel.Lifted.Combinators

import Data.Array.Parallel.Base ( showsApp )

import Prelude (Int, Double, Show(..), (-))
import qualified Prelude as P

class Elt a where
  pa :: PA a

instance Elt Int where
  pa = dPA_Int

instance Elt Double where
  pa = dPA_Double

instance (Elt a, Elt b) => Elt (a,b) where
  pa = dPA_2 pa pa

instance (Elt a, Elt b, Elt c) => Elt (a,b,c) where
  pa = dPA_3 pa pa pa

instance (Elt a, Elt b, Elt c, Elt d) => Elt (a,b,c,d) where
  pa = dPA_4 pa pa pa pa

instance (Elt a, Elt b, Elt c, Elt d, Elt e) => Elt (a,b,c,d,e) where
  pa = dPA_5 pa pa pa pa pa

length :: Elt a => PArray a -> Int
{-# INLINE length #-}
length = lengthPA_v pa

empty :: Elt a => PArray a
{-# INLINE empty #-}
empty = emptyPA pa

replicate :: Elt a => Int -> a -> PArray a
{-# INLINE replicate #-}
replicate = replicatePA_v pa

singleton :: Elt a => a -> PArray a
{-# INLINE singleton #-}
singleton = singletonPA_v pa

zip :: (Elt a, Elt b) => PArray a -> PArray b -> PArray (a,b)
{-# INLINE zip #-}
zip = zipPA_v pa pa

unzip :: (Elt a, Elt b) => PArray (a,b) -> (PArray a, PArray b)
{-# INLINE unzip #-}
unzip = unzipPA_v pa pa

enumFromTo :: Int -> Int -> PArray Int
{-# INLINE enumFromTo #-}
enumFromTo = enumFromToPA_v

fromList :: Elt a => [a] -> PArray a
{-# INLINE fromList #-}
fromList = fromListPA pa

toList :: Elt a => PArray a -> [a]
toList xs = [indexPA_v pa xs i | i <- [0 .. length xs - 1]]

nf :: Elt a => PArray a -> ()
nf = nfPA pa

instance (Elt a, Show a) => Show (PArray a) where
  showsPrec n xs = showsApp n "fromList<PArray>" (toList xs)

