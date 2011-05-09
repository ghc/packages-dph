
-- | Parallel Arrays.
--
--   Parallel arrays use a fixed generic representation. All data stored in
--   them is converted to the generic representation, and we have a small
--   number of operators that work on arrays of these generic types.
--
--   Representation types include Ints, Floats, Tuples and Sums, so arrays of
--   these types can be stored directly. However, user defined algebraic data
--   needs to be converted as we don't have operators that work directly on
--   arrays of these types.
--
--   The top-level PArray type is built up from several type families and
--   clases:
--
--     PArray        - This is the top level type. It holds an array length, 
--                     and array data in the generic representation (PData).
--
--     PRepr         - Family of types that can be converted to the generic
--                     representation. We supply instances for basic types
--                     like Ints Floats etc, but the vectoriser needs to make
--                     the instances for user-defined data types itself.
--      PA class     - Contains methods to convert to and from the generic
--                     representation (PData).
-- 
--     PData         - Family of types that can be stored directly in parallel
--                     arrays. We supply all the PData instances we need here
--                     in the library.
--      PR class     - Contains methods that work directly on parallel arrays.
--                     Most of these are just wrappers for the corresponding
--                     U.Array operators.
--
--     Scalar class  - Contains methods to convert between the generic 
--                     representation (PData) and plain U.Arrays.
--
--  Note that the PRepr family and PA class are related.
--     so are the PData family and PR class.
--
--  For motivational material see:
--    "An Approach to Fast Arrays in Haskell", Chakravarty and Keller, 2003
--
--  For discussion of how the mapping to generic types works see:
--    "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
--
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

import Prelude          hiding ( length, replicate, zip, unzip, enumFromTo )

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

