{-# LANGUAGE BangPatterns #-}
module Flow (smvm) where
import Data.Vector.Unboxed
import Data.Array.Repa.Flow             (Flow)
import qualified Data.Array.Repa.Flow   as F
import qualified Data.Vector.Unboxed    as U


smvm    :: Vector Int           -- ^ Row lengths for matrix.
        -> Vector (Int, Double) -- ^ Sparse matrix column number and coefficient.
        -> Vector Double        -- ^ Dense vector.
        -> IO (Vector Double)

smvm !vLens !vMatrix !vVector
 = do   let (vColId, vColVal)   = U.unzip vMatrix
        fLens           <- F.flow vLens
        fColId          <- F.flow vColId
        fColVal         <- F.flow vColVal
        F.unflow        $  smvm' fLens fColId fColVal vVector

smvm' fLens fColId fColVal !vVector
        = F.sums fLens 
        $ F.zipWith (*) fColVal 
        $ F.gather vVector fColId
{-# INLINE smvm' #-}
