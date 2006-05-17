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
  unitsU, loopU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), runST)
import Data.Array.Parallel.Base.Fusion (
  EFL)
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr,
  lengthU, indexU, unitsU,
  newMU, writeMU, unsafeFreezeMU)

-- |Iteration over over non-nested arrays
--
loopU :: (UA e, UA e')
      => EFL acc e e'                      -- mapping & folding, once per elem
      -> acc				   -- initial acc value
      -> UArr e			           -- input array
      -> (UArr e' :*: acc)
{-# INLINE [1] loopU #-}
loopU mf start a =
  a `seq`            -- GHC can't figure out that loopU is strict in the array
                     -- on its own which means that loopU isn't considered
                     -- an interesting continuation. This prevents, e.g.,
                     -- replicateU in
                     --   g n = mapU f . replicateU n
                     -- from being inlined and subsequently fused.
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

