{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
module RankVectorised        
        (ranksPA)
where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int
import Data.Array.Parallel.Prelude.Bool
import qualified Prelude as P


ranksPA :: PArray Int -> PArray Int
{-# NOINLINE ranksPA #-}
ranksPA ps
        = toPArrayP (ranks (fromPArrayP ps))

ranks :: [:Int:] -> [:Int:]
{-# NOINLINE ranks #-}
ranks arr = [: lengthP [: a | a <- arr, a < b :] | b <- arr :]

