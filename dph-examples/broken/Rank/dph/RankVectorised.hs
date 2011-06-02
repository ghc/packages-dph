{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module RankVectorised        
        (ranksPA)
where
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int
import qualified Prelude as P


ranksPA :: PArray Int -> PArray Int
{-# NOINLINE ranksPA #-}
ranksPA ps
        = toPArrayP (ranks (fromPArrayP ps))

ranks :: [:Int:] -> [:Int:]
{-# NOINLINE ranks #-}
ranks arr = [: lengthP [: a | a <- arr, a < b :] | b <- arr :]

