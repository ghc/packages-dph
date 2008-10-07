{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.UArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (GADTS)
--
-- Description ---------------------------------------------------------------
--
-- This module defines unlifted arrays generically as a GADT.
--
-- Slicing is implemented by each `BUArr' having the slicing information.  A
-- possible alternative design would be to maintain this information in
-- `UArr', but not in the representations, but at the root.  This may seem
-- attractive at first, but seems to be more disruptive without any real
-- benefits _ this is essentially, because we then need the slicing
-- information at each level; ie, also at the leafs where it is sufficient
-- using the current implementation.
--
-- Todo ----------------------------------------------------------------------
--

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Flat.UArr (

  -- * Array types and classes containing the admissable elements types
  UA, UArr, MUArr, {-USel(..), MUSel(..),-}

  -- * Basic operations on parallel arrays
  lengthU, indexU, sliceU, {-extractU,-} unitsU, zipU, unzipU, fstU, sndU,
  newU, newDynU, newDynResU,
  lengthMU, newMU, readMU, writeMU, copyMU, unsafeFreezeMU, unsafeFreezeAllMU,
  hasAtomicWriteMU, atomicWriteMU,


  -- * I\/O
  UIO(..)

) where

-- standard libraries
import Control.Monad (liftM, liftM2)
import GHC.Word      (Word8)

-- friends
import Data.Array.Parallel.Base
import Data.Array.Parallel.Arr (
  BUArr, MBUArr, UAE,
  lengthBU, indexBU, sliceBU, hGetBU, hPutBU,
  lengthMBU, newMBU, readMBU, writeMBU, copyMBU, unsafeFreezeMBU)

import System.IO

infixl 9 `indexU`, `readMU`


-- |Basic operations on representation types
-- -----------------------------------------

-- |This type class determines the types that can be elements immutable
-- unboxed arrays. The representation type of these arrays is defined by way
-- of an associated type.  All representation-dependent functions are methods
-- of this class.
--
class HS e => UA e where
  data UArr  e
  data MUArr e :: * -> * 

  -- |Yield the length of an unboxed array
  lengthU        :: UArr e                     -> Int

  -- |Extract an element out of an immutable unboxed array
  indexU         :: UArr e -> Int              -> e

  -- |Restrict access to a subrange of the original array (no copying)
  sliceU         :: UArr e -> Int -> Int       -> UArr e

  -- |Yield the length of a mutable unboxed array
  lengthMU       :: MUArr e s                  -> Int

  -- |Allocate a mutable unboxed array
  newMU          :: Int                        -> ST s (MUArr e s)

  -- |Read an element from a mutable unboxed array
  readMU         :: MUArr e s -> Int           -> ST s e

  -- |Update an element in a mutable unboxed array
  writeMU        :: MUArr e s -> Int -> e      -> ST s ()

  -- |Indicate whether the type supports atomic updates
  hasAtomicWriteMU :: e -> Bool

  -- |Atomically update an element in a mutable unboxed array if supported
  atomicWriteMU  :: MUArr e s -> Int -> e      -> ST s ()

  -- |Copy the contents of an immutable unboxed array into a mutable one
  -- from the specified position on
  copyMU         :: MUArr e s -> Int -> UArr e -> ST s ()

  -- |Convert a mutable into an immutable unboxed array
  unsafeFreezeMU :: MUArr e s -> Int           -> ST s (UArr e)


  hasAtomicWriteMU _  = False
  atomicWriteMU _ _ _ = error "atomicWriteMU: not supported"

instance HS e => HS (UArr e)
instance HS e => HS (MUArr e s)

class UAE e => UPrim e where
  mkUAPrim :: BUArr e -> UArr  e
  unUAPrim :: UArr  e -> BUArr e

  mkMUAPrim :: MBUArr s e -> MUArr  e s
  unMUAPrim :: MUArr  e s -> MBUArr s e

unsafeFreezeAllMU :: UA e => MUArr e s -> ST s (UArr e)
unsafeFreezeAllMU marr = unsafeFreezeMU marr (lengthMU marr)

-- |Creating unboxed arrays
-- ------------------------

newU :: UA e => Int -> (forall s. MUArr e s -> ST s ()) -> UArr e
{-# INLINE_U newU #-}
newU n init = newDynU n (\ma -> init ma >> return n)

newDynU :: UA e => Int -> (forall s. MUArr e s -> ST s Int) -> UArr e
{-# INLINE_U newDynU #-}
newDynU n init =
  runST (do
           ma <- newMU n
           n' <- init ma
           unsafeFreezeMU ma n'
  )

newDynResU :: UA e
           => Int -> (forall s. MUArr e s -> ST s (Int :*: r)) -> UArr e :*: r
{-# INLINE_U newDynResU #-}
newDynResU n init =
  runST (do
           ma <- newMU n
           n' :*: r <- init ma
           arr <- unsafeFreezeMU ma n'
           return (arr :*: r)
  )

-- |Basic operations on unboxed arrays
-- -----------------------------------

-- |Yield an array of units 
--
unitsU :: Int -> UArr ()
{-# INLINE_STREAM unitsU #-}
unitsU = UAUnit

-- |Elementwise pairing of array elements.
--
zipU :: (UA a, UA b) => UArr a -> UArr b -> UArr (a :*: b)
{-# INLINE_STREAM zipU #-}
zipU = UAProd

-- |Elementwise unpairing of array elements.
--
unzipU :: (UA a, UA b) => UArr (a :*: b) -> (UArr a :*: UArr b)
{-# INLINE_STREAM unzipU #-}
unzipU (UAProd l r) = (l :*: r)

-- |Yield the first components of an array of pairs.
--
fstU :: (UA a, UA b) => UArr (a :*: b) -> UArr a
{-# INLINE_STREAM fstU #-}
fstU (UAProd l r) = l

-- |Yield the second components of an array of pairs.
--
sndU :: (UA a, UA b) => UArr (a :*: b) -> UArr b
{-# INLINE_STREAM sndU #-}
sndU (UAProd l r) = r

-- |Family of representation types
-- -------------------------------

-- |Array operations on the unit representation.
--
instance UA () where
  newtype UArr  ()   = UAUnit  Int
  newtype MUArr () s = MUAUnit Int

  lengthU (UAUnit n)     = n
  indexU  (UAUnit _) _   = ()
  sliceU  (UAUnit _) _ n = UAUnit n

  lengthMU (MUAUnit n)            = n
  newMU   n                       = return $ MUAUnit n
  readMU (MUAUnit _) _            = return ()
  writeMU (MUAUnit _) _ _         = return ()
  copyMU (MUAUnit _) _ (UAUnit _) = return ()
  unsafeFreezeMU (MUAUnit _) n    = return $ UAUnit n

  hasAtomicWriteMU _ = True
  atomicWriteMU      = writeMU

-- |Array operations on the pair representation.
--
instance (UA a, UA b) => UA (a :*: b) where
  data UArr  (a :*: b)   = UAProd  !(UArr a)    !(UArr b)
  data MUArr (a :*: b) s = MUAProd !(MUArr a s) !(MUArr b s)

  -- TODO: changed from (lengthU l), as this causes problems when the length is used to
  --       limit the index
  lengthU (UAProd l r)     = checkEq "lengthU" "lengths of zipped arrays differ" (lengthU l) (lengthU r)
     (lengthU l) 
  {-# INLINE_U indexU #-}
  indexU  (UAProd l r) i   = indexU l i :*: indexU r i
  {-# INLINE_U sliceU #-}
  sliceU  (UAProd l r) i n = UAProd (sliceU l i n) (sliceU r i n)

  {-# INLINE_U lengthMU #-}
  lengthMU (MUAProd l r)   = lengthMU l

  {-# INLINE_U newMU #-}
  newMU n = 
    do
      a <- newMU n
      b <- newMU n
      return $ MUAProd a b
  {-# INLINE_U readMU #-}
  readMU (MUAProd a b) i = liftM2 (:*:) (a `readMU` i) (b `readMU` i)
  {-# INLINE_U writeMU #-}
  writeMU (MUAProd a b) i (x :*: y) = 
    do
      writeMU a i x
      writeMU b i y
  {-# INLINE_U copyMU #-}
  copyMU (MUAProd ma mb) i (UAProd a b) =
    do
      copyMU ma i a
      copyMU mb i b
  {-# INLINE_U unsafeFreezeMU #-}
  unsafeFreezeMU (MUAProd a b) n = 
    do
      a' <- unsafeFreezeMU a n
      b' <- unsafeFreezeMU b n
      return $ UAProd a' b'

{-
-- |Selector for immutable arrays of sums
--
data USel = USel {
	      selUS  :: !(BUArr Bool),  -- selector (False => left)
	      lidxUS :: !(BUArr Int),   -- left indices
	      ridxUS :: !(BUArr Int)    -- right indices
	    }

instance HS USel

-- |Selector for mutable arrays of sums
--
data MUSel s = MUSel {
	         selMUS  :: !(MBUArr s Bool),  -- selector (False => left)
	         lidxMUS :: !(MBUArr s Int),   -- left indices
	         ridxMUS :: !(MBUArr s Int)    -- right indices
	       }

instance HS (MUSel s)

-- |Array operations on the sum representation
--
instance (UA a, UA b) => UA (a :+: b) where
  lengthU (UASum sel _ _)     = lengthBU (selUS sel)
  {-# INLINE_U indexU #-}
  indexU  (UASum sel l r) i   = if (selUS sel)`indexBU`i then Inr $ indexU r i 
					 	         else Inl $ indexU l i
  {-# INLINE_U sliceU #-}
  sliceU  (UASum sel l r) i n = 
    let
      sel'        = sliceBU (selUS sel) i n
      li          = lidxUS sel`indexBU`i
      ri          = ridxUS sel`indexBU`i
      lidx        = mapBU (subtract li) $ sliceBU (lidxUS sel) i n
      ridx        = mapBU (subtract ri) $ sliceBU (ridxUS sel) i n
      (ln :*: rn) = if n == 0 then (0 :*: 0) else (lidx`indexBU`(n - 1) :*: 
					           ridx`indexBU`(n - 1))
    in
    UASum (USel sel' lidx ridx) (sliceU l li ln) (sliceU r ri rn)
  {-# INLINE_U extractU #-}
  extractU  (UASum sel l r) i n = 
    let
      sel'        = extractBU (selUS sel) i n
      li          = lidxUS sel`indexBU`i
      ri          = ridxUS sel`indexBU`i
      lidx        = mapBU (subtract li) $ sliceBU (lidxUS sel) i n
      ridx        = mapBU (subtract ri) $ sliceBU (ridxUS sel) i n
      (ln :*: rn) = if n == 0 then (0 :*: 0) else (lidx`indexBU`(n - 1) :*: 
					           ridx`indexBU`(n - 1))
    in
    UASum (USel sel' lidx ridx) (extractU l li ln) (extractU r ri rn)

instance (MUA a, MUA b) => MUA (a :+: b) where
  {-# INLINE_U newMU #-}
  newMU n = do
	      sel  <- newMBU n
	      lidx <- newMBU n
	      ridx <- newMBU n
	      a    <- newMU n
	      b    <- newMU n
	      return $ MUASum (MUSel sel lidx ridx) a b
  {-# INLINE_U writeMU #-}
  writeMU (MUASum sel l r) i (Inl x) = 
    do
      let lidx = lidxMUS sel
	  ridx = ridxMUS sel
      writeMBU (selMUS sel) i False
      li <- if i == 0 then return 0 else liftM (+ 1) $ lidx`readMBU`(i - 1)
      ri <- if i == 0 then return 0 else	       ridx`readMBU`(i - 1)
      writeMBU lidx i li
      writeMBU ridx i ri
      writeMU l li x
  writeMU (MUASum sel l r) i (Inr x) = 
    do
      let lidx = lidxMUS sel
	  ridx = ridxMUS sel
      writeMBU (selMUS sel) i True
      li <- if i == 0 then return 0 else               lidx`readMBU`(i - 1)
      ri <- if i == 0 then return 0 else liftM (+ 1) $ ridx`readMBU`(i - 1)
      writeMBU lidx i li
      writeMBU ridx i ri
      writeMU r ri x
    --FIXME: that works only when the array is constructed left to right, but
    --not for something like permutations
  {-# INLINE_U unsafeFreezeMU #-}
  unsafeFreezeMU (MUASum sel l r) n = 
    do
      sel' <- unsafeFreezeMBU (selMUS  sel) n
      lidx <- unsafeFreezeMBU (lidxMUS sel) n
      ridx <- unsafeFreezeMBU (ridxMUS sel) n
      let ln = if n == 0 then 0 else lidx`indexBU`(n - 1)
	  rn = if n == 0 then 0 else ridx`indexBU`(n - 1)
      l' <- unsafeFreezeMU l ln
      r' <- unsafeFreezeMU r rn
      return $ UASum (USel sel' lidx ridx) l' r'
-}

-- |Array operations on unboxed arrays
-- -
--
-- NB: We use instances for all possible unboxed types instead of re-using the
--     overloading provided by UAE to avoid having to store the UAE dictionary
--     in `UAPrimU'.

primLengthU :: UPrim e => UArr e -> Int
{-# INLINE_U primLengthU #-}
primLengthU = lengthBU . unUAPrim

primIndexU :: UPrim e => UArr e -> Int -> e
{-# INLINE_U primIndexU #-}
primIndexU = indexBU . unUAPrim

primSliceU :: UPrim e => UArr e -> Int -> Int -> UArr e
{-# INLINE_U primSliceU #-}
primSliceU arr i = mkUAPrim . sliceBU (unUAPrim arr) i

primLengthMU :: UPrim e => MUArr e s -> Int
{-# INLINE_U primLengthMU #-}
primLengthMU = lengthMBU . unMUAPrim

primNewMU :: UPrim e => Int -> ST s (MUArr e s)
{-# INLINE_U primNewMU #-}
primNewMU = liftM mkMUAPrim . newMBU

primReadMU :: UPrim e => MUArr e s -> Int -> ST s e
{-# INLINE_U primReadMU #-}
primReadMU = readMBU . unMUAPrim

primWriteMU :: UPrim e => MUArr e s -> Int -> e -> ST s ()
{-# INLINE_U primWriteMU #-}
primWriteMU = writeMBU . unMUAPrim

primCopyMU :: UPrim e => MUArr e s -> Int -> UArr e -> ST s ()
{-# INLINE_U primCopyMU #-}
primCopyMU ma i = copyMBU (unMUAPrim ma) i . unUAPrim

primUnsafeFreezeMU :: UPrim e => MUArr e s -> Int -> ST s (UArr e)
{-# INLINE_U primUnsafeFreezeMU #-}
primUnsafeFreezeMU ma = liftM mkUAPrim . unsafeFreezeMBU (unMUAPrim ma)

instance UPrim Bool where
  mkUAPrim                = UABool
  unUAPrim  (UABool  arr) = arr

  mkMUAPrim               = MUABool
  unMUAPrim (MUABool arr) = arr

instance UA Bool where
  newtype UArr  Bool   = UABool  (BUArr Bool)
  newtype MUArr Bool s = MUABool (MBUArr s Bool)

  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UPrim Char where
  mkUAPrim                 = UAChar
  unUAPrim  (UAChar   arr) = arr

  mkMUAPrim                = MUAChar
  unMUAPrim (MUAChar arr)  = arr

instance UA Char where
  newtype UArr  Char   = UAChar  (BUArr Char)
  newtype MUArr Char s = MUAChar (MBUArr s Char)

  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UPrim Int where
  mkUAPrim               = UAInt
  unUAPrim  (UAInt  arr) = arr

  mkMUAPrim              = MUAInt
  unMUAPrim (MUAInt arr) = arr

instance UA Int where
  newtype UArr  Int   = UAInt  (BUArr Int)
  newtype MUArr Int s = MUAInt (MBUArr s Int)

  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

  -- FIXME: For now, we assume that Int writes are atomic but we should really
  --        configure this.

  hasAtomicWriteMU _ = True
  atomicWriteMU      = primWriteMU

instance UPrim Word8 where
  mkUAPrim               = UAWord8
  unUAPrim  (UAWord8  arr) = arr

  mkMUAPrim              = MUAWord8
  unMUAPrim (MUAWord8 arr) = arr

instance UA Word8 where
  newtype UArr  Word8   = UAWord8  (BUArr Word8)
  newtype MUArr Word8 s = MUAWord8 (MBUArr s Word8)

  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

  -- FIXME: For now, we assume that Word8 writes are atomic but we should really
  --        configure this.

  hasAtomicWriteMU _ = True
  atomicWriteMU      = primWriteMU

instance UPrim Float where
  mkUAPrim                 = UAFloat
  unUAPrim  (UAFloat  arr) = arr

  mkMUAPrim                = MUAFloat
  unMUAPrim (MUAFloat arr) = arr

instance UA Float where
  newtype UArr  Float   = UAFloat  (BUArr Float)
  newtype MUArr Float s = MUAFloat (MBUArr s Float)

  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UPrim Double where
  mkUAPrim                  = UADouble
  unUAPrim  (UADouble  arr) = arr

  mkMUAPrim                 = MUADouble
  unMUAPrim (MUADouble arr) = arr

instance UA Double where
  newtype UArr  Double   = UADouble  (BUArr Double)
  newtype MUArr Double s = MUADouble (MBUArr s Double)

  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

-- * I\/O
-- -----

class UA a => UIO a where
  hPutU :: Handle -> UArr a -> IO ()
  hGetU :: Handle -> IO (UArr a)

primPutU :: UPrim a => Handle -> UArr a -> IO ()
primPutU h = hPutBU h . unUAPrim

primGetU :: UPrim a => Handle -> IO (UArr a)
primGetU = liftM mkUAPrim . hGetBU

instance UIO Int where
  hPutU = primPutU
  hGetU = primGetU

instance UIO Double where
  hPutU = primPutU
  hGetU = primGetU

instance (UIO a, UIO b) => UIO (a :*: b) where
  hPutU h (UAProd xs ys) = do hPutU h xs
                              hPutU h ys
  hGetU h                = do xs <- hGetU h
                              ys <- hGetU h
                              return (UAProd xs ys)

