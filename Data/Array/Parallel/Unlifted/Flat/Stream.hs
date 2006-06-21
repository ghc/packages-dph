module Data.Array.Parallel.Unlifted.Flat.Stream (
  streamU, unstreamU
) where

import Data.Array.Parallel.Base (
  runST)
import Data.Array.Parallel.Stream (
  Step(..), Stream(..))
import Data.Array.Parallel.Unlifted.Flat.UArr (
  UArr, UA, indexU, lengthU, newMU, unsafeFreezeMU, writeMU)

-- | Generate a stream from an array, from left to right
--
streamU :: UA a => UArr a -> Stream a
{-# INLINE [1] streamU #-}
streamU arr = Stream next 0 n
  where
    n = lengthU arr
    next i | i == n    = Done
           | otherwise = Yield (arr `indexU` i) (i+1)

-- | Create an array from a stream, filling it from left to right
--
unstreamU :: UA a => Stream a -> UArr a
{-# INLINE [1] unstreamU #-}
unstreamU (Stream next s n) =
  runST (do
           marr <- newMU n
           n'   <- fill0 marr
           unsafeFreezeMU marr n'
  )
  where
    fill0 marr = fill s 0
      where
        fill s i = i `seq`      -- the seq seems to be necessary here
                   case next s of
                     Done       -> return i
                       -- The return () is necessary because GHC won't
                       -- eta-expand fill otherwise, building a giant
                       -- ST closure instead of just performing the writes.
                     Skip s'    -> return () >> fill s' i
                     Yield x s' -> do
                                     writeMU marr i x
                                     fill s' (i+1)
-- | Fusion rules
-- --------------

{-# RULES  -- -} (for font-locking)

"streamU/unstreamU" forall s.
  streamU (unstreamU s) = s
 
  #-}

