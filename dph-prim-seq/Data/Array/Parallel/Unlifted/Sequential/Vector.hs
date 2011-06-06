{-# LANGUAGE ScopedTypeVariables, BangPatterns, CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Vector (

  -- * Array classes
  Unbox,

  -- * Array types
  Vector, MVector,

  -- * Streaming
  stream, unstream,

  -- * Basic operations
  length, null, empty, singleton, cons, units,
  replicate,
  -- replicateEachU,
  (!), (++),
  interleave, indexed, repeat, repeatS,

  -- * Subarrays
  slice, extract,
  tail,
  take, drop, splitAt,

  -- * Permutations
  permute, bpermute, mbpermute, bpermuteDft, reverse, update,


  -- * Higher-order operations
  map, zipWith, zipWith3,
  filter, pack, 
  combine, combine2ByTag,
  foldl, foldl1, foldl1Maybe,
  fold, fold1, fold1Maybe,
  scanl, scanl1,
  scan, scan1,
  scanRes,

  -- * Searching
  elem, notElem,

  -- * Logical operations
  and, or, any, all,

  -- * Arithmetic operations
  sum, product,
  maximum, minimum,
  maximumBy, minimumBy,
  maxIndex, minIndex,
  maxIndexBy, minIndexBy,

  -- * Arrays of pairs
  zip, unzip, fsts, snds,

  -- * Enumerations
  enumFromTo, enumFromThenTo, enumFromStepLen, enumFromToEach, enumFromStepLenEach,

  -- * Searching
  find, findIndex,

  -- * Conversions to\/from lists
  toList, fromList,

  -- * Random arrays
  random, randomR,

  -- * Mutating operations
  new, copy,

  -- * Mutable vectors
  newM, unsafeFreeze, M.write, M.read, mpermute, mupdate,
  mdrop, mslice,

  -- * I\/O
  UIO(..)

) where

import Data.Array.Parallel.Stream
import Data.Array.Parallel.Base ( Tag, checkEq, ST )

import Data.Vector.Unboxed hiding ( slice, zip, unzip, foldl, foldl1, scanl, scanl1 )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed.Base as VBase
import Data.Vector.Generic ( stream, unstream )
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Storable.Mutable as MStorable
import qualified Data.Vector.Generic.New as New
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..) )
import Data.Vector.Fusion.Stream.Size ( Size(..) )
import Prelude hiding ( length, null,
                        replicate, (++), repeat,
                        tail, take, drop,
                        reverse,
                        map, zipWith, zipWith3, filter,
                        foldl, foldl1, scanl, scanl1,
                        elem, notElem,
                        and, or, any, all,
                        sum, product,
                        maximum, minimum,
                        zip, unzip,
                        enumFromTo, enumFromThenTo )
import qualified Prelude
import qualified System.Random as R
import Foreign hiding ( new )
import System.IO

here s = "Data.Array.Parallel.Unlifted.Sequential.Flat." Prelude.++ s

new :: Unbox a => Int -> (forall s. MVector s a -> ST s ()) -> Vector a
{-# INLINE new #-}
new n p = V.create (do
                      v <- M.new n
                      p v
                      return v)

newM :: Unbox a => Int -> ST s (MVector s a)
{-# INLINE newM #-}
newM = M.new

-- |Yield an array of units 
--
units :: Int -> Vector ()
{-# INLINE units #-}
units n = replicate n ()
                        
-- |Interleave the elements of two arrays
--
interleave :: Unbox e => Vector e -> Vector e -> Vector e
{-# INLINE_U interleave #-}
interleave xs ys = unstream (interleaveS (stream xs) (stream ys))

-- |Associate each element of the array with its index
--
indexed :: Unbox e => Vector e -> Vector (Int,e)
{-# INLINE_U indexed #-}
indexed = unstream . indexedS . stream

-- |Repeat an array @n@ times
--
repeat :: Unbox e => Int -> Vector e -> Vector e
{-# INLINE_U repeat #-}
repeat n xs = unstream (repeatS n xs)

repeatS :: Unbox e => Int -> Vector e -> S.Stream e
{-# INLINE_STREAM repeatS #-}
repeatS k !xs = Stream next (0,k) (Exact (k*n))
  where
    !n = length xs

    {-# INLINE next #-}
    next (i,0) = return Done
    next (i,k) | i == n    = return $ Skip                     (0,k-1)
               | otherwise = return $ Yield (unsafeIndex xs i) (i+1,k)

{-# INLINE_U slice #-}
slice :: Unbox a => Vector a -> Int -> Int -> Vector a
slice xs i n = V.slice i n xs

{-# INLINE_U extract #-}
extract :: Unbox a => Vector a -> Int -> Int -> Vector a
extract xs i n = force (V.slice i n xs)

mupdate :: Unbox e => MVector s e -> Vector (Int,e) -> ST s ()
{-# INLINE_U mupdate #-}
mupdate marr xs = MG.update marr (stream xs)

mpermute :: Unbox e => MVector s e -> Vector e -> Vector Int -> ST s ()
{-# INLINE_U mpermute #-}
mpermute marr xs is = MG.update marr (stream (zip is xs))

-- |Standard permutation
--
permute :: Unbox e => Vector e -> Vector Int -> Vector e
{-# INLINE_U permute #-}
permute xs is = create (do
                          v <- M.new (length xs)
                          mpermute v xs is
                          return v)

bpermute :: Unbox e => Vector e -> Vector Int -> Vector e
{-# INLINE_U bpermute #-}
bpermute = backpermute

mbpermute :: (Unbox e, Unbox d) => (e -> d) -> Vector e -> Vector Int -> Vector d
{-# INLINE_STREAM mbpermute #-}
mbpermute f es is  = unstream (mbpermuteS f es (stream is))

bpermuteS :: Unbox e => Vector e -> S.Stream Int -> S.Stream e
{-# INLINE_STREAM bpermuteS #-}
bpermuteS !a s = S.map (a!) s

mbpermuteS:: Unbox e => (e -> d) -> Vector e -> S.Stream Int -> S.Stream d
{-# INLINE_STREAM mbpermuteS #-}
mbpermuteS f !a = S.map (f . (a!))

-- |Default back permute
--
-- * The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
-- * All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
--
bpermuteDft :: Unbox e
	    => Int			        -- ^ length of result array
	    -> (Int -> e)		        -- ^ initialiser function
	    -> Vector (Int,e)	        	-- ^ index-value pairs
	    -> Vector e
{-# INLINE_U bpermuteDft #-}
bpermuteDft n init = update (map init (enumFromN 0 n))

-- |Extract all elements from an array according to a given flag array
-- 
pack:: Unbox e => Vector e -> Vector Bool -> Vector e
{-# INLINE_U pack #-}
pack xs = map fst . filter snd . zip xs

combine :: Unbox a
	 => Vector Bool -> Vector a -> Vector a -> Vector a
{-# INLINE combine #-}
combine bs = combine2ByTag (map (\b -> if b then 0 else 1) bs)

combine2ByTag :: Unbox a => Vector Tag -> Vector a -> Vector a -> Vector a
{-# INLINE_U combine2ByTag #-}
combine2ByTag ts xs ys
  = checkEq (here "combine2ByTag")
            ("tags length /= sum of args length")
            (length ts) (length xs + length ys)
  $ unstream (combine2ByTagS (stream ts) (stream xs) (stream ys))


-- |Array reduction proceeding from the left
--
foldl :: Unbox a => (b -> a -> b) -> b -> Vector a -> b
{-# INLINE_U foldl #-}
foldl = foldl'

-- |Array reduction proceeding from the left for non-empty arrays
--
foldl1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE_U foldl1 #-}
foldl1 = foldl1'

-- |Array reduction that requires an associative combination function with its
-- unit
--
fold :: Unbox a => (a -> a -> a) -> a -> Vector a -> a
{-# INLINE_U fold #-}
fold = foldl

-- |Reduction of a non-empty array which requires an associative combination
-- function
--
fold1 :: Unbox a => (a -> a -> a) -> Vector a -> a
{-# INLINE_U fold1 #-}
fold1 = foldl1


foldl1Maybe :: Unbox a => (a -> a -> a) -> Vector a -> Maybe a
{-# INLINE_U foldl1Maybe #-}
foldl1Maybe f xs = foldl' join Nothing xs
  where
    {-# INLINE join #-}
    join Nothing  y = Just $! y
    join (Just x) y = Just $! f x y

fold1Maybe :: Unbox a => (a -> a -> a) -> Vector a -> Maybe a
{-# INLINE_U fold1Maybe #-}
fold1Maybe = foldl1Maybe

-- |Prefix scan proceedings from left to right
--
scanl :: (Unbox a, Unbox b) => (b -> a -> b) -> b -> Vector a -> Vector b
{-# INLINE_U scanl #-}
scanl = prescanl'

-- |Prefix scan of a non-empty array proceeding from left to right
--
scanl1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE_U scanl1 #-}
scanl1 = scanl1'

-- |Prefix scan proceeding from left to right that needs an associative
-- combination function with its unit
--
scan :: Unbox a => (a -> a -> a) -> a -> Vector a -> Vector a
{-# INLINE_U scan #-}
scan = scanl

-- |Prefix scan of a non-empty array proceeding from left to right that needs
-- an associative combination function
--
scan1 :: Unbox a => (a -> a -> a) -> Vector a -> Vector a
{-# INLINE_U scan1 #-}
scan1 = scanl1

scanRes :: Unbox a => (a -> a -> a) -> a -> Vector a -> (Vector a,a)
{-# INLINE_U scanRes #-}
scanRes f z xs = let ys = scanl' f z xs
                 in
                 (unsafeInit ys, unsafeLast ys)

fsts :: (Unbox a, Unbox b) => Vector (a,b) -> Vector a
{-# INLINE_STREAM fsts #-}
fsts (VBase.V_2 _ xs ys) = xs

snds :: (Unbox a, Unbox b) => Vector (a,b) -> Vector b
{-# INLINE_STREAM snds #-}
snds (VBase.V_2 _ xs ys) = ys

zip :: (Unbox a, Unbox b) => Vector a -> Vector b -> Vector (a,b)
{-# INLINE_STREAM zip #-}
zip !xs !ys = V.zip xs ys

unzip :: (Unbox a, Unbox b) => Vector (a,b) -> (Vector a, Vector b)
{-# INLINE_STREAM unzip #-}
unzip ps = V.unzip ps

{-# RULES

"fsts/new/unstream [dph-prim-seq]" forall xs.
  fsts (G.new (New.unstream xs)) = V.map fst (G.new (New.unstream xs))

"snds/new/unstream [dph-prim-seq]" forall xs.
  snds (G.new (New.unstream xs)) = V.map snd (G.new (New.unstream xs))

"stream/zip [dph-prim-seq]" forall xs ys.
  G.stream (zip xs ys) = S.zip (G.stream xs) (G.stream ys)

  #-}

enumFromStepLen :: Int -> Int -> Int -> Vector Int
{-# INLINE_U enumFromStepLen #-}
enumFromStepLen = enumFromStepN

enumFromToEach :: Int -> Vector (Int,Int) -> Vector Int
{-# INLINE_U enumFromToEach #-}
enumFromToEach n = unstream . enumFromToEachS n . stream

enumFromStepLenEach :: Int -> Vector Int -> Vector Int -> Vector Int -> Vector Int
{-# INLINE_U enumFromStepLenEach #-}
enumFromStepLenEach len starts steps lens
  = unstream $ enumFromStepLenEachS len $ stream $ V.zip3 starts steps lens


random :: (Unbox a, R.Random a, R.RandomGen g) => Int -> g -> Vector a
{-# INLINE_U random #-}
random n = unstream . randomS n

randomR :: (Unbox a, R.Random a, R.RandomGen g) => Int -> (a,a) -> g -> Vector a
{-# INLINE_U randomR #-}
randomR n r = unstream . randomRS n r

randomS :: (R.RandomGen g, R.Random a) => Int -> g -> S.Stream a
{-# INLINE_STREAM randomS #-}
randomS n g = Stream step (g,n) (Exact n)
  where
    {-# INLINE step #-}
    step (g,0) = return Done
    step (g,n) = let (x,g') = R.random g
                 in return $ Yield x (g',n-1)

randomRS :: (R.RandomGen g, R.Random a) => Int -> (a,a) -> g -> S.Stream a
{-# INLINE_STREAM randomRS #-}
randomRS n r g = Stream step (g,n) (Exact n)
  where
    {-# INLINE step #-}
    step (g,0) = return Done
    step (g,n) = let (x,g') = R.randomR r g
                 in return $ Yield x (g',n-1)

mdrop :: Unbox a => Int -> MVector s a -> MVector s a
{-# INLINE mdrop #-}
mdrop = M.drop

mslice :: Unbox a => Int -> Int -> MVector s a -> MVector s a
{-# INLINE mslice #-}
mslice = M.slice

-- * I\/O
-- -----

hGetStorable :: forall a. Storable a => Handle -> IO (Storable.Vector a)
hGetStorable h =
  alloca $ \iptr ->
  do
    hGetBuf h iptr (sizeOf (undefined :: Int))
    n <- peek iptr
    v <- MStorable.unsafeNew n
    let bytes = sizeOf (undefined :: a) * MStorable.length v
    r <- MStorable.unsafeWith v $ \ptr -> hGetBuf h ptr bytes
    Storable.unsafeFreeze (MStorable.take r v)

hPutStorable :: forall a. Storable a => Handle -> Storable.Vector a -> IO ()
hPutStorable h xs =
  alloca $ \iptr ->
  do
    poke iptr n 
    hPutBuf h iptr (sizeOf n)
    Storable.unsafeWith xs $ \ptr ->
      do
        hPutBuf h ptr (sizeOf (undefined :: a) * n)
        return ()
  where
    !n = Storable.length xs

class Unbox a => UIO a where
  hPut :: Handle -> Vector a -> IO ()
  hGet :: Handle -> IO (Vector a)

primPut :: (Unbox a, Storable a) => Handle -> Vector a -> IO ()
{-# INLINE primPut #-}
primPut h = hPutStorable h . Storable.convert

primGet :: (Unbox a, Storable a) => Handle -> IO (Vector a)
{-# INLINE primGet #-}
primGet = fmap convert . hGetStorable

instance UIO Int where
  {-# INLINE hPut #-}
  hPut = primPut
  {-# INLINE hGet #-}
  hGet = primGet

instance UIO Double where
  {-# INLINE hPut #-}
  hPut = primPut
  {-# INLINE hGet #-}
  hGet = primGet

instance (UIO a, UIO b) => UIO (a,b) where
  {-# INLINE hPut #-}
  hPut h xs = case V.unzip xs of
                (ys,zs) -> do hPut h ys
                              hPut h zs

  {-# INLINE hGet #-}
  hGet h = do xs <- hGet h
              ys <- hGet h
              return (V.zip xs ys)


