{-# OPTIONS -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.UArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
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

module Data.Array.Parallel.Unlifted.Flat.UArr (

  -- * Array types and classes containing the admissable elements types
  UA, UArr, MUArr, {-USel(..), MUSel(..),-}

  -- * Basic operations on parallel arrays
  lengthU, indexU, sliceU, {-extractU,-} unitsU, zipU, unzipU, fstU, sndU,
  newU, newDynU,
  lengthMU, newMU, readMU, writeMU, copyMU, unsafeFreezeMU, unsafeFreezeAllMU

) where

-- standard libraries
import Monad (liftM, liftM2)

-- friends
import Data.Array.Parallel.Base
import Data.Array.Parallel.Arr (
  BUArr, MBUArr, UAE,
  lengthBU, indexBU, sliceBU,
  lengthMBU, newMBU, readMBU, writeMBU, copyMBU, unsafeFreezeMBU)

infixl 9 `indexU`, `readMU`


-- |Basic operations on representation types
-- -----------------------------------------

-- |This type class determines the types that can be elements immutable
-- unboxed arrays. The representation type of these arrays is defined by way
-- of an associated type.  All representation-dependent functions are methods
-- of this class.
--
class HS e => UA e where
--  data UArr e
--  data MUArr e s

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

  -- |Copy the contents of an immutable unboxed array into a mutable one
  -- from the specified position on
  copyMU         :: MUArr e s -> Int -> UArr e -> ST s ()

  -- |Convert a mutable into an immutable unboxed array
  unsafeFreezeMU :: MUArr e s -> Int           -> ST s (UArr e)

-- GADT TO REPLACE AT FOR THE MOMENT
data UArr e where
  UAUnit :: !Int                               -> UArr ()
  UAProd ::           !(UArr e1) -> !(UArr e2) -> UArr (e1 :*: e2)
--  UASum  :: !USel  -> !(UArr e1) -> !(UArr e2) -> UArr (e1 :+: e2)
  UAPrim ::           !(BUArr e)                -> UArr e

instance HS e => HS (UArr e)

-- GADT TO REPLACE AT FOR THE MOMENT
data MUArr e s where
  MUAUnit :: !Int                                          -> MUArr () s
  MUAProd ::                !(MUArr e1 s) -> !(MUArr e2 s) -> MUArr (e1 :*: e2) s
--  MUASum  :: !(MUSel s)  -> !(MUArr e1 s) -> !(MUArr e2 s) -> MUArr (e1 :+: e2) s
  MUAPrim ::                !(MBUArr s e)                   -> MUArr e s

instance HS e => HS (MUArr e s)

unUAPrim :: UAE e => UArr e -> BUArr e
unUAPrim (UAPrim arr) = arr

unMUAPrim :: UAE e => MUArr e s -> MBUArr s e
unMUAPrim (MUAPrim arr) = arr

unsafeFreezeAllMU :: UA e => MUArr e s -> ST s (UArr e)
unsafeFreezeAllMU marr = unsafeFreezeMU marr (lengthMU marr)

-- |Creating unboxed arrays
-- ------------------------

newU :: UA e => Int -> (forall s. MUArr e s -> ST s ()) -> UArr e
{-# INLINE newU #-}
newU n init = newDynU n (\ma -> init ma >> return n)

newDynU :: UA e => Int -> (forall s. MUArr e s -> ST s Int) -> UArr e
{-# INLINE newDynU #-}
newDynU n init =
  runST (do
           ma <- newMU n
           n' <- init ma
           unsafeFreezeMU ma n'
  )

-- |Basic operations on unboxed arrays
-- -----------------------------------

-- |Yield an array of units 
--
unitsU :: Int -> UArr ()
{-# INLINE [1] unitsU #-}
unitsU = UAUnit

-- |Elementwise pairing of array elements.
--
zipU :: (UA a, UA b) => UArr a -> UArr b -> UArr (a :*: b)
{-# INLINE [1] zipU #-}
zipU = UAProd

-- |Elementwise unpairing of array elements.
--
unzipU :: (UA a, UA b) => UArr (a :*: b) -> (UArr a :*: UArr b)
{-# INLINE [1] unzipU #-}
unzipU (UAProd l r) = (l :*: r)

-- |Yield the first components of an array of pairs.
--
fstU :: (UA a, UA b) => UArr (a :*: b) -> UArr a
{-# INLINE [1] fstU #-}
fstU (UAProd l r) = l

-- |Yield the second components of an array of pairs.
--
sndU :: (UA a, UA b) => UArr (a :*: b) -> UArr b
{-# INLINE [1] sndU #-}
sndU (UAProd l r) = r

-- |Family of representation types
-- -------------------------------

-- |Array operations on the unit representation.
--
instance UA () where
  lengthU (UAUnit n)     = n
  indexU  (UAUnit _) _   = ()
  sliceU  (UAUnit _) _ n = UAUnit n

  lengthMU (MUAUnit n)            = n
  newMU   n                       = return $ MUAUnit n
  readMU (MUAUnit _) _            = return ()
  writeMU (MUAUnit _) _ _         = return ()
  copyMU (MUAUnit _) _ (UAUnit _) = return ()
  unsafeFreezeMU (MUAUnit _) n    = return $ UAUnit n

-- |Array operations on the pair representation.
--
instance (UA a, UA b) => UA (a :*: b) where
  lengthU (UAProd l _)     = lengthU l
  {-# INLINE indexU #-}
  indexU  (UAProd l r) i   = indexU l i :*: indexU r i
  {-# INLINE sliceU #-}
  sliceU  (UAProd l r) i n = UAProd (sliceU l i n) (sliceU r i n)

  {-# INLINE lengthMU #-}
  lengthMU (MUAProd l r)   = lengthMU l

  {-# INLINE newMU #-}
  newMU n = 
    do
      a <- newMU n
      b <- newMU n
      return $ MUAProd a b
  {-# INLINE readMU #-}
  readMU (MUAProd a b) i = liftM2 (:*:) (a `readMU` i) (b `readMU` i)
  {-# INLINE writeMU #-}
  writeMU (MUAProd a b) i (x :*: y) = 
    do
      writeMU a i x
      writeMU b i y
  {-# INLINE copyMU #-}
  copyMU (MUAProd ma mb) i (UAProd a b) =
    do
      copyMU ma i a
      copyMU mb i b
  {-# INLINE unsafeFreezeMU #-}
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
  {-# INLINE indexU #-}
  indexU  (UASum sel l r) i   = if (selUS sel)`indexBU`i then Inr $ indexU r i 
					 	         else Inl $ indexU l i
  {-# INLINE sliceU #-}
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
  {-# INLINE extractU #-}
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
  {-# INLINE newMU #-}
  newMU n = do
	      sel  <- newMBU n
	      lidx <- newMBU n
	      ridx <- newMBU n
	      a    <- newMU n
	      b    <- newMU n
	      return $ MUASum (MUSel sel lidx ridx) a b
  {-# INLINE writeMU #-}
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
  {-# INLINE unsafeFreezeMU #-}
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

primLengthU :: UAE e => UArr e -> Int
{-# INLINE primLengthU #-}
primLengthU = lengthBU . unUAPrim

primIndexU :: UAE e => UArr e -> Int -> e
{-# INLINE primIndexU #-}
primIndexU = indexBU . unUAPrim

primSliceU :: UAE e => UArr e -> Int -> Int -> UArr e
{-# INLINE primSliceU #-}
primSliceU arr i = UAPrim . sliceBU (unUAPrim arr) i

primLengthMU :: UAE e => MUArr e s -> Int
{-# INLINE primLengthMU #-}
primLengthMU = lengthMBU . unMUAPrim

primNewMU :: UAE e => Int -> ST s (MUArr e s)
{-# INLINE primNewMU #-}
primNewMU = liftM MUAPrim . newMBU

primReadMU :: UAE e => MUArr e s -> Int -> ST s e
{-# INLINE primReadMU #-}
primReadMU = readMBU . unMUAPrim

primWriteMU :: UAE e => MUArr e s -> Int -> e -> ST s ()
{-# INLINE primWriteMU #-}
primWriteMU = writeMBU . unMUAPrim

primCopyMU :: UAE e => MUArr e s -> Int -> UArr e -> ST s ()
{-# INLINE primCopyMU #-}
primCopyMU ma i = copyMBU (unMUAPrim ma) i . unUAPrim

primUnsafeFreezeMU :: UAE e => MUArr e s -> Int -> ST s (UArr e)
{-# INLINE primUnsafeFreezeMU #-}
primUnsafeFreezeMU ma = liftM UAPrim . unsafeFreezeMBU (unMUAPrim ma)

instance UA Bool where
  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UA Char where
  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UA Int where
  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UA Float where
  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

instance UA Double where
  lengthU        = primLengthU
  indexU         = primIndexU
  sliceU         = primSliceU

  lengthMU       = primLengthMU
  newMU          = primNewMU
  readMU         = primReadMU
  writeMU        = primWriteMU
  copyMU         = primCopyMU
  unsafeFreezeMU = primUnsafeFreezeMU

