-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Loop
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (GADTS)
--
-- Description ---------------------------------------------------------------
--
-- Loop/replicate combinators for flat arrays
--
-- /WARNING:/ If you import this, you probably also want to
--            import Data.Array.Parallel.Unlifted.Flat.Fusion
--            to get the fusion rules.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Loop (
  unitsU, replicateU, loopU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), runST)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr(UAUnit),
  lengthU, indexU,
  newMU, writeMU, unsafeFreezeMU)

-- |Yield an array of units 
--
unitsU :: Int -> UArr ()
unitsU = UAUnit

-- |Yield an array where all elements contain the same value
--
replicateU :: UA e => Int -> e -> UArr e
{-# INLINE [1] replicateU #-}
replicateU n e = 
  runST (do
    mpa <- newMU n
    fill0 mpa
    unsafeFreezeMU mpa n
  )
  where
    fill0 mpa = fill 0
      where
        fill i | i == n    = return ()
	       | otherwise = writeMU mpa i e >> fill (i + 1)

-- |Iteration over over non-nested arrays
--
loopU :: (UA e, UA e')
      => (acc -> e -> (acc :*: Maybe e'))  -- mapping & folding, once per elem
      -> acc				   -- initial acc value
      -> UArr e			           -- input array
      -> (UArr e' :*: acc)
{-# INLINE [1] loopU #-}
loopU mf start a = 
  runST (do
    mpa            <- newMU len
    (acc :*: len') <- trans0 mpa start
    a'             <- unsafeFreezeMU mpa len'
    return (a' :*: acc)
  )
  where
    len = lengthU a
    --
    trans0 ma start = trans 0 0 start
      where
        trans a_off ma_off acc 
	  | a_off == len = ma_off `seq`	       -- needed for these arguments...
			   acc    `seq`	       -- ...to get unboxed
			   return (acc :*: ma_off)
	  | otherwise    =
	    do
	      let (acc' :*: oe) = mf acc (a `indexU` a_off)
	      ma_off' <- case oe of
			   Nothing -> return ma_off
			   Just e  -> do
				        writeMU ma ma_off e
					return $ ma_off + 1
	      trans (a_off + 1) ma_off' acc'

