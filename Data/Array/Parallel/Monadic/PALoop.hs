-- |Loop combinators on parallel arrays
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  $Id: PALoop.hs,v 1.11 2003/03/13 05:24:44 chak Exp $
--
--  This file may be used, modified, and distributed under the same conditions
--  and the same warranty disclaimer as set out in the X11 license.
--
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98 + multi-parameter classes
--
--  Loop/replicate combinators
--
--  We do not directly use `PAEP.EP' here, but instead export the less generic
--  combinators and leave it to `PAEP' to generalise them.  This avoids
--  some problems with resolving class constraints in functions that don't
--  have an explicit element type in their signature (eg, `replicateSP).
--
--- Todo ----------------------------------------------------------------------
--

module PALoop (
  -- * Loop/replicate combinators
  replicateP, loopP, replicateSP, loopSP,

  -- * Projection combinators for loops
  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc
) where

-- GHC-specific modules
import Data.Generics

-- friends
import Data.Array.Parallel.Base.UArr
              (UArr, lengthU, indexU, loopArr, loopAcc, loopSndAcc, sumU,
	       scanU, ST, runST)
import PABase (PArray, FArray, PArr, SPArr, MSPArr, PArrInt, PAPrimU(..),
	       PAPArr(..), Segd(..), 
	       lengthP, indexP, sliceP, newMP, newMSP, writeMP, nextMSP,
	       unsafeFreezeMP, unsafeFreezeMSP)


-- |Elementary combinators
-- -----------------------

-- |Yield an array where all elements contain the same value
--
replicateP :: FArray r arr => Int -> r -> PArr arr e
{-# INLINE [1] replicateP #-}
replicateP n e = 
  runST (do
    mpa <- newMP n
    fill0 mpa
    unsafeFreezeMP mpa n
  )
  where
    fill0 mpa = fill 0
      where
        fill i | i == n    = return ()
	       | otherwise = writeMP mpa i e >> fill (i + 1)

-- |Iteration over over non-nested arrays
--
loopP :: (PArray r arr, FArray r' arr')
      => (acc -> r -> (acc, Maybe r'))  -- mapping & folding, once per element
      -> acc				-- initial acc value
      -> PArr arr e			-- input array
      -> (PArr arr' e', acc)
{-# INLINE [1] loopP #-}
loopP mf start a = 
  runST (do
    mpa         <- newMP len
    (acc, len') <- trans0 mpa start
    a'          <- unsafeFreezeMP mpa len'
    return (a', acc)
  )
  where
    len = lengthP a
    --
    trans0 ma start = trans 0 0 start
      where
        trans a_off ma_off acc 
	  | a_off == len = ma_off `seq`	       -- needed for these arguments...
			   acc    `seq`	       -- ...to get unboxed
			   return (acc, ma_off)
	  | otherwise    =
	    do
	      let (acc', oe) = mf acc (a `indexP` a_off)
	      ma_off' <- case oe of
			   Nothing -> return ma_off
			   Just e  -> do
				        writeMP ma ma_off e
					return $ ma_off + 1
	      trans (a_off + 1) ma_off' acc'


-- |Segmented combinators
-- ----------------------

-- |Segmented replication
--
replicateSP :: FArray r arr => PArrInt -> PArr arr e -> SPArr arr e
{-# INLINE [1] replicateSP #-}
replicateSP (PAPrimU ns) es = 
  runST (do
    let n    = sumU ns
        psum = scanU (+) 0 ns
    mpa  <- newMP n
    fillSegs0 mpa
    pa   <- unsafeFreezeMP mpa n
    return $ PAPArr (Segd ns psum) pa
  )
  where
    m = lengthU ns
    --
    fillSegs0 mpa = fillSegs 0 0
      where
        fillSegs i j 
	  | i == m    = return ()
	  | otherwise = do
			  j' <- fill j (ns `indexU` i) (es `indexP` i)
			  fillSegs (i + 1) j'
	--
	fill j 0 e = return j
	fill j k e = writeMP mpa j e >> fill (j + 1) (k - 1) e

-- |Segmented iterator
--
-- * A segmented loop is controlled by two mutators, which are executed once
--   per element of the input array and once per segment end, respectively.
--
-- * The element mutator gets the running accumulator value and current
--   element and, in addition to updating the accumulator, may produce
--   an output element.
--
-- * The segment mutator gets the running accumulator and the segment number,
--   and, in addition to updating the accumulator, may produce an entry for
--   the accumulator array.
--
loopSP :: (PArray r arr, FArray r' arr', FArray ar aarr)
       => (acc -> r   -> (acc, Maybe r'))    -- per element mutator
       -> (acc -> Int -> (acc, Maybe ar))    -- per segment mutator
       -> acc				     -- initial acc value
       -> SPArr arr e			     -- input array
       -> ((SPArr arr' e', PArr aarr ae), acc)
-- FIXME: Intro a debug flag that activates segd conistency checks
{-# INLINE [1] loopSP #-}
loopSP em sm start (PAPArr segd arr) =
  runST (do
    mpa      <- newMSP nsegd n
    maccs    <- newMP        nsegd
    (m, acc) <- trans0 mpa maccs
    arr'     <- unsafeFreezeMSP mpa   nsegd
    accs     <- unsafeFreezeMP  maccs m
    return ((arr', accs), acc)
  )
  where
    segd1 = segdS segd
    nsegd = lengthU segd1
    n     = lengthP arr
    --
    trans0 mpa maccs = trans 0 0 (-1) 0 start
      where
        trans arr_i	-- index into the flat input array
	      seg_cnt   -- number of elements still to be done in current seg
	      segd_i	-- currently processed segment (source and dest)
	      maccs_i   -- next index to write to in the accumulator array
	      acc       -- accumulator
	  | seg_cnt == 0 =	-- start a new segment
	    arr_i   `seq`
	    maccs_i `seq`
	    acc     `seq`
	    do
	      (maccs_i', acc') <- 
	        if segd_i == -1		-- before first segment starts
		then return (maccs_i, acc)
		else 
		  case sm acc segd_i of
		    (acc', Nothing) -> return (maccs_i, acc')
		    (acc', Just ae) -> do
				         writeMP maccs maccs_i ae
					 return (maccs_i + 1, acc')
	      let segd_i'  = segd_i + 1		-- next segment
	      maccs_i' `seq` 
	       acc'    `seq` 
	       if segd_i' == nsegd	-- no more segments left
	        then 
		  return (maccs_i', acc')
		else do
		  let seg_cnt' = segd1 `indexU` segd_i' -- get new seg length
		  nextMSP mpa segd_i' Nothing           -- init target seg
		  trans arr_i seg_cnt' segd_i' maccs_i' acc'
          | otherwise    =      -- continue with current segment
	    segd_i  `seq`
	    maccs_i `seq`
	    do
	      let (acc', oe) = em acc (arr `indexP` arr_i)
	      case oe of
	        Nothing -> return ()
		Just e  -> nextMSP mpa segd_i (Just e)
              trans (arr_i + 1) (seg_cnt - 1) segd_i maccs_i acc'

-- |Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
--
loopArrS :: ((arr, accs), acc) -> arr
{-# INLINE [1] loopArrS #-}
loopArrS ((arr, _), _) = arr
--
loopAccS :: ((arr, accs), acc) -> accs
{-# INLINE [1] loopAccS #-}
loopAccS ((_, accs), _) = accs
