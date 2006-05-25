-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Loop
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
-- Loop combinator and fusion rules for flat arrays
--
-- Todo ----------------------------------------------------------------------
--
--  * Add more fusion rules
--

module Data.Array.Parallel.Unlifted.Flat.Loop (
  unitsU, loopU
) where

import Data.Array.Parallel.Base (
  (:*:)(..), MaybeS(..), runST)
import Data.Array.Parallel.Base.Fusion
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
    trans0 ma start = flattrans 0 0 start
      where
        flattrans a_off ma_off acc 
	  | a_off == len = ma_off `seq`	       -- needed for these arguments...
			   acc    `seq`	       -- ...to get unboxed
			   return (acc :*: ma_off)
	  | otherwise    =
	    do
	      let (acc' :*: oe) = mf acc (a `indexU` a_off)
	      ma_off' <- case oe of
			   NothingS -> return ma_off
			   JustS e  -> do
				         writeMU ma ma_off e
					 return $ ma_off + 1
	      flattrans (a_off + 1) ma_off' acc'

-- Fusion rules
-- ------------

-- Rules for unsegmented loops
--

{-# RULES  -- -} (for font-locking)

"loopU/loopU" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    loopSndAcc (loopU (em1 `fuseEFL` em2) (start1 :*: start2) arr)

-- fusion across a zipU
--
-- * we can only fuse across a zip if the the fused loop does not drop any
--   elements; the case of a `mapEFL' guarantees this and is easy to recognise
--
-- * note that, due to `mapEFL', we are ignoring the accumulator; so, we can
--   just forget about the accumulator of the inner loop
{- FIXME: typing problem
"loopU/zipU/loopU-left" forall f1 start1 em2 start2 pa1 pa2.
  loopU em2 start2 (zipU (loopArr (loopU (mapEFL f1) start1 pa1)) pa2) =
    let
      em acc2 (e1 :*: e2) = 
        case em2 acc2 (f1 e1 :*: e2) of
	  (acc2' :*: Nothing ) -> (acc2' :*: Nothing )
	  (acc2' :*: Just e'') -> (acc2' :*: Just e'')
    in
    loopU em start2 (zipU pa1 pa2)

"loopU/zipU/loopU-right" forall f1 start1 em2 start2 pa1 pa2.
  loopU em2 start2 (zipU pa2 (loopArr (loopU (mapEFL f1) start1 pa1))) =
    let
      em acc2 (e1 :*: e2) = 
        case em2 acc2 (e1 :*: f1 e2) of
	  (acc2' :*: Nothing ) -> (acc2' :*: Nothing )
	  (acc2' :*: Just e'') -> (acc2' :*: Just e'')
    in
    loopU em start2 (zipU pa1 pa2)
-}
--FIXME: similar zip rules work for scanEFL, too

 #-}

