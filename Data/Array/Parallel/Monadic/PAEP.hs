-- |Embedding projection pairs as a binary type class
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  $Id: PAEP.hs,v 1.15 2002/12/02 07:42:40 chak Exp $
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 + multi-parameter classes & functional dependencies
--
--  This module defines embedding projection pairs generically as a
--  two-parameter class.  It also generalises some functions from `PALoop' to
--  use EPs.
--
--- Todo ----------------------------------------------------------------------
--
--  * Can't properly use `:=:' instead of `EP' as infix classes names aren't
--    yet properly parsed in context (with GHC 5.04)
--

module PAEP (
  -- * Type class for embedding projection pairs
  EP(from, to),

  -- * Re-export EPified `PABase' functions
  PABase.lengthP, indexP, PABase.sliceP, PABase.newMP, PABase.newMSP, writeMP,
  nextMSP, PABase.unsafeFreezeMP, (PABase.>:), PABase.flattenP,

  -- * Re-export EPified `PALoop' functions
  replicateP, loopP, PALoop.replicateSP, loopSP
) where

-- GHC-specific modules
import Data.Generics

-- friends
import Data.Array.Parallel.Base.UArr  (UArr, MUArr, ST)
import PABase    (PArray, FArray, PArr, MPArr, SPArr, MSPArr)
import qualified
       PABase    (lengthP, indexP, sliceP, newMP, newMSP, writeMP, nextMSP,
		  unsafeFreezeMP, (>:), flattenP) 
import qualified
       PALoop    (replicateP, loopP, replicateSP, loopSP)


-- |Conversion between concrete and representation types
-- -----------------------------------------------------

-- |Embedding projection pairs
--
class EP t r | t -> r where
  from :: t -> r
  to   :: r -> t

-- |Instantiations
-- -

-- |
instance EP Unit Unit where
  from = id
  to   = id

-- |
instance EP () Unit where
  from () = Unit
  to Unit = ()

-- |
instance (EP a ar, EP b br) => EP (a :*: b) (ar :*: br) where
  from (x :*: y) = (from x :*: from y)
  to   (x :*: y) = (to x   :*: to   y)

-- |
instance (EP a ar, EP b br) => EP (a, b) (ar :*: br) where
  from (x, y)  = from x :*: from y
  to (x :*: y) = (to x, to y)

-- |
instance (EP a ar, EP b br, EP c cr) => EP (a, b, c) (ar :*: br :*: cr) where
  from (x, y, z)     = from x :*: from y :*: from z
  to (x :*: y :*: z) = (to x, to y, to z)

-- |
instance (EP a ar, EP b br) => EP (a :+: b) (ar :+: br) where
  from (Inl x) = (Inl $ from x)
  from (Inr y) = (Inr $ from y)
  to   (Inl x) = (Inl $ to x)
  to   (Inr y) = (Inr $ to y)

-- |
instance EP Bool Bool where
  from = id
  to   = id

-- |
instance EP Char Char where
  from = id
  to   = id

-- |
instance EP Int Int where
  from = id
  to   = id

-- |
instance EP Float Float where
  from = id
  to   = id

-- |
instance EP Double Double where
  from = id
  to   = id

-- |
instance PArray r arr => EP (PArr arr e) (PArr arr e) where
  from = id
  to   = id
  --FIXME: Do we need to descent here?


-- |Generic variants of functions from `PABase' & `PALoop'
-- -------------------------------------------------------

indexP  :: (EP e r, PArray r arr) => PArr arr e -> Int -> e
{-# INLINE indexP #-}
indexP arr i = to $ arr `PABase.indexP` i

writeMP :: (EP e r, FArray r arr) => MPArr s arr e -> Int -> e -> ST s ()
{-# INLINE writeMP #-}
writeMP arr i e = PABase.writeMP arr i (from e)

nextMSP :: (EP e r, FArray r arr) 
	=> MSPArr s arr e -> Int -> Maybe e -> ST s ()
{-# INLINE nextMSP #-}
nextMSP arr i e = PABase.nextMSP arr i (fmap from e)

-- |Yield an array where all elements contain the same value
--
replicateP :: (EP e r, FArray r arr) => Int -> e -> PArr arr e
{-# INLINE [1] replicateP #-}
replicateP i e = PALoop.replicateP i (from e)

-- |Iteration over over non-nested arrays
--
loopP :: (EP e r, PArray r arr, EP e' r', FArray r' arr')
      => (acc -> e -> (acc, Maybe e'))  -- mapping & folding, once per element
      -> acc				-- initial acc value
      -> PArr arr e 			-- input array
      -> (PArr arr' e', acc)
{-# INLINE [1] loopP #-}
loopP mf = PALoop.loopP mf'
  where
    mf' acc r = case mf acc (to r) of
		  (acc', Nothing) -> (acc', Nothing)
		  (acc', Just r') -> (acc', Just $ from r')

-- |Segmented iterator
--
loopSP :: (EP e  r , PArray r  arr, 
	   EP e' r', FArray r' arr', 
	   EP ae ar, FArray ar aarr)
       => (acc -> e   -> (acc, Maybe e'))    -- per element mutator
       -> (acc -> Int -> (acc, Maybe ae))    -- per segment mutator
       -> acc				     -- initial acc value
       -> SPArr arr e			     -- input array
       -> ((SPArr arr' e', PArr aarr ae), acc)
{-# INLINE [1] loopSP #-}
loopSP em sm = PALoop.loopSP em' sm'
  where
    em' acc r = case em acc (to r) of
		  (acc', Nothing) -> (acc', Nothing)
		  (acc', Just r') -> (acc', Just $ from r')
    --
    sm' acc i = case sm acc i of
		  (acc', Nothing) -> (acc', Nothing)
		  (acc', Just r') -> (acc', Just $ from r')

