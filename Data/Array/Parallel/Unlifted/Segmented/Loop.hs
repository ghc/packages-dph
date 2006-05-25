-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Segmented.Loop
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
-- Loop/replicate combinators and fusion rules for segmented arrays
--
-- Todo ----------------------------------------------------------------------
--
--  * Add more fusion rules
--

module Data.Array.Parallel.Unlifted.Segmented.Loop (
  replicateSU, loopSU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), runST)
import Data.Array.Parallel.Base.Fusion (
  EFL, SFL, mapEFL, scanEFL, loopArr, loopSndAcc)
import Data.Array.Parallel.Unlifted.Flat (
  UA, UArr,
  lengthU, (!:), loopU, sumU, scanU,
  newMU, writeMU, unsafeFreezeMU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr(
  SUArr(..), USegd(..),
  (>:),
  newMSU, nextMSU, unsafeFreezeMSU)
  

-- |Segmented replication
--
replicateSU :: UA e => UArr Int -> UArr e -> SUArr e
{-# INLINE [1] replicateSU #-}
replicateSU ns es = 
  runST (do
    let n    = sumU ns
        psum = scanU (+) 0 ns
    mpa  <- newMU n
    fillSegs0 mpa
    pa   <- unsafeFreezeMU mpa n
    return $ SUArr (USegd ns psum) pa
  )
  where
    m = lengthU ns
    --
    fillSegs0 mpa = fillSegs 0 0
      where
        fillSegs i j 
	  | i == m    = return ()
	  | otherwise = do
			  j' <- fill j (ns !: i) (es !: i)
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
       => EFL acc e e'                          -- per element mutator
       -> SFL acc ae                            -- per segment mutator
       -> acc				        -- initial acc value
       -> SUArr e			        -- input array
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
    nsegd = lengthU segd1
    n     = lengthU arr
    --
    trans0 mpa maccs = segtrans 0 0 (-1) 0 start
      where
        segtrans arr_i	-- index into the flat input array
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
		    (acc' :*: NothingS) -> return (maccs_i :*: acc')
		    (acc' :*: JustS ae) -> do
				             writeMU maccs maccs_i ae
					     return (maccs_i + 1 :*: acc')
	      let segd_i'  = segd_i + 1		-- next segment
	      maccs_i' `seq` 
	       acc'    `seq` 
	       if segd_i' == nsegd	-- no more segments left
	        then 
		  return (maccs_i' :*: acc')
		else do
		  let seg_cnt' = segd1 !: segd_i' -- get new seg length
		  nextMSU mpa segd_i' NothingS    -- init target seg
		  segtrans arr_i seg_cnt' segd_i' maccs_i' acc'
          | otherwise    =      -- continue with current segment
	    segd_i  `seq`
	    maccs_i `seq`
	    do
	      let (acc' :*: oe) = em acc (arr !: arr_i)
	      case oe of
	        NothingS -> return ()
		JustS e  -> nextMSU mpa segd_i (JustS e)
              segtrans (arr_i + 1) (seg_cnt - 1) segd_i maccs_i acc'

-- Fusion rules
-- ------------

-- Rules combining segmented and unsegmented loops
--

{-# RULES  -- -} (for font-locking)

--FIXME: would it be better to match on PAPArr instead of on ">:"?
--       Problem mit matching auf PAPArr is that GHC inserts some sort of
--	 $wPAPArr, which messes everything up.  Can we prevent this?
"loopSU/loopU[map]" forall f1 em2 sm2 segd start1 start2 arr.
  loopSU em2 sm2 start2 (segd >: loopArr (loopU (mapEFL f1) start1 arr)) =
    let
      em acc2 e = em2 acc2 (f1 e)
    in
    loopSU em sm2 start2 (segd >: arr)

"loopSU/loopU[scan]" forall f1 em2 sm2 segd start1 start2 arr.
  loopSU em2 sm2 start2 (segd >: loopArr (loopU (scanEFL f1) start1 arr)) =
    let
      em (acc1 :*: acc2) e = 
        case em2 acc2 acc1 of
	  (acc2' :*: res) -> ((f1 acc1 e :*: acc2') :*: res)
      --
      sm (acc1 :*: acc2) i = 
        case sm2 acc2 i of
	  (acc2' :*: res) -> ((acc1 :*: acc2') :*: res)
    in
    loopSndAcc $ loopSU em sm (start1 :*: start2) (segd >: arr)

 #-}

-- Rules for segmented loops
--

-- {-# RULES  -- -} (for font-locking)

-- #-}


