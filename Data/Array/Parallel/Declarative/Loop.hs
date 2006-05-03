-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Declarative.Loop
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
-- Loop/replicate combinators
--
-- We do not directly use `PAEP.EP' here, but instead export the less generic
-- combinators and leave it to `PAEP' to generalise them.  This avoids
-- some problems with resolving class constraints in functions that don't
-- have an explicit element type in their signature (eg, `replicateSP).
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Declarative.Loop (

  -- * Loop/replicate combinators
  unitsU, replicateU, loopU, replicateSU, loopSU,

  -- * Projection combinators for loops
--  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc

) where

-- friends
import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Base.Prim
import Data.Array.Parallel.Base.Fusion (
  loopArr, loopArrS, loopAcc, loopAccS, loopSndAcc)
import Data.Array.Parallel.Base.BUArr (
  BUArr, lengthBU, indexBU, loopArr, loopAcc, loopSndAcc, sumBU, scanBU, 
  ST, runST) 
import Data.Array.Parallel.Monadic.UArr (
  UA, UArr(..), MUArr, 
  lengthU, indexU, newMU, writeMU, unsafeFreezeMU)
import Data.Array.Parallel.Monadic.SUArr (
  USegd(..), MUSegd(..), SUArr(..), MSUArr(..),
  newMSU, nextMSU, unsafeFreezeMSU)


-- |Elementary combinators
-- -----------------------

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


-- |Segmented combinators
-- ----------------------

-- |Segmented replication
--
replicateSU :: UA e => UArr Int -> UArr e -> SUArr e
{-# INLINE [1] replicateSU #-}
replicateSU (UAPrim (PrimInt ns)) es = 
  runST (do
    let n    = sumBU ns
        psum = scanBU (+) 0 ns
    mpa  <- newMU n
    fillSegs0 mpa
    pa   <- unsafeFreezeMU mpa n
    return $ SUArr (USegd ns psum) pa
  )
  where
    m = lengthBU ns
    --
    fillSegs0 mpa = fillSegs 0 0
      where
        fillSegs i j 
	  | i == m    = return ()
	  | otherwise = do
			  j' <- fill j (ns `indexBU` i) (es `indexU` i)
			  fillSegs (i + 1) j'
	--
	fill j 0 e = return j
	fill j k e = writeMU mpa j e >> fill (j + 1) (k - 1) e

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
loopSU :: (UA e, UA e', UA ae)
       => (acc -> e   -> (acc :*: Maybe e'))    -- per element mutator
       -> (acc -> Int -> (acc :*: Maybe ae))    -- per segment mutator
       -> acc				        -- initial acc value
       -> SUArr e			       -- input array
       -> ((SUArr e' :*: UArr ae) :*: acc)
-- FIXME: Intro a debug flag that activates segd conistency checks
{-# INLINE [1] loopSU #-}
loopSU em sm start (SUArr segd arr) =
  runST (do
    mpa         <- newMSU nsegd n
    maccs       <- newMU        nsegd
    (m :*: acc) <- trans0 mpa maccs
    arr'        <- unsafeFreezeMSU mpa   nsegd
    accs        <- unsafeFreezeMU  maccs m
    return ((arr' :*: accs) :*: acc)
  )
  where
    segd1 = segdUS segd
    nsegd = lengthBU segd1
    n     = lengthU arr
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
	      (maccs_i' :*: acc') <- 
	        if segd_i == -1		-- before first segment starts
		then return (maccs_i :*: acc)
		else 
		  case sm acc segd_i of
		    (acc' :*: Nothing) -> return (maccs_i :*: acc')
		    (acc' :*: Just ae) -> do
				            writeMU maccs maccs_i ae
					    return (maccs_i + 1 :*: acc')
	      let segd_i'  = segd_i + 1		-- next segment
	      maccs_i' `seq` 
	       acc'    `seq` 
	       if segd_i' == nsegd	-- no more segments left
	        then 
		  return (maccs_i' :*: acc')
		else do
		  let seg_cnt' = segd1 `indexBU` segd_i' -- get new seg length
		  nextMSU mpa segd_i' Nothing            -- init target seg
		  trans arr_i seg_cnt' segd_i' maccs_i' acc'
          | otherwise    =      -- continue with current segment
	    segd_i  `seq`
	    maccs_i `seq`
	    do
	      let (acc' :*: oe) = em acc (arr `indexU` arr_i)
	      case oe of
	        Nothing -> return ()
		Just e  -> nextMSU mpa segd_i (Just e)
              trans (arr_i + 1) (seg_cnt - 1) segd_i maccs_i acc'

