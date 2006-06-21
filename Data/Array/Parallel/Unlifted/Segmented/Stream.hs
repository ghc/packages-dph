module Data.Array.Parallel.Unlifted.Segmented.Stream (
  streamSU, unstreamSU
) where

import Data.Array.Parallel.Base (
  (:+:)(..),
  runST)
import Data.Array.Parallel.Stream (
  Step(..), SStream(..), segmentS)
import Data.Array.Parallel.Unlifted.Flat (
  UA, streamU, writeMU)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  SUArr(..), MSUArr(..), MUSegd(..),
  segdUS, (>:),
  newMSU, unsafeFreezeMSU)

streamSU :: UA a => SUArr a -> SStream a
{-# INLINE [1] streamSU #-}
streamSU (SUArr segd arr) = segmentS (streamU (segdUS segd)) (streamU arr)

unstreamSU :: UA a => SStream a -> SUArr a
{-# INLINE [1] unstreamSU #-}
unstreamSU (SStream next s nsegs n) =
  runST (do
           msu <- newMSU nsegs n
           trans0 msu
           unsafeFreezeMSU msu nsegs
  )
  where
    trans0 (MSUArr (MUSegd segd psum) arr) = trans s 0 0
      where
        trans s k i =
          case next s of
            Done             -> return ()
              -- See comments in unstreamU about why return () is necessary
              -- here.
            Skip s'          -> return () >> trans s' k i
            Yield (Inl l) s' -> do
                                  writeMU segd k l
                                  writeMU psum k i
                                  trans s' (k+1) i
            Yield (Inr x) s' -> do
                                  writeMU arr i x
                                  trans s' k (i+1)

{-# RULES -- -}

"streamSU/unstreamSU" forall s.
  streamSU (unstreamSU s) = s

"streamSU/segment" forall segd arr.
  streamSU (segd >: arr) = segmentS (streamU (segdUS segd)) (streamU arr)

 #-}

