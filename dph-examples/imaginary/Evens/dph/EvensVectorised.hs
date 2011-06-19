{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module EvensVectorised (evensPA) where
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Int
import qualified Prelude as P

-- | DPH filter opereations are reasonably involved because they use packByTag.
evens :: [:Int:] -> [:Int:]
evens ints = filterP (\x -> x `mod` 2 == 0) ints

evensPA :: PArray Int -> PArray Int
{-# NOINLINE evensPA #-}
evensPA arr = toPArrayP (evens (fromPArrayP arr))

