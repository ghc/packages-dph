#include "fusion-phases.h"

module Data.Array.Parallel.PArray.Sums
        ( sumPA_double, sumPA_l_double
        , sumPA_int,    sumPA_l_int)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.Scalar
import qualified Data.Vector                            as V
import qualified Data.Array.Parallel.Unlifted           as U


-- Double ---------------------------------------------------------------------
{-# INLINE_PA sumPA_double #-}
sumPA_double   :: PArray Double -> Double
sumPA_double (PArray _ (PDouble xs))
        = U.sum xs


-- | Lifted sum for Doubles
sumPA_l_double 
        :: Int
        -> PData (PArray Double)
        -> PData Double
{-# NOINLINE sumPA_l_double #-}        
sumPA_l_double _ (PNested vsegd datas)
 = {-# SCC "sumPA_l_double" #-}
   let  -- Grab all the flat source vectors.
        !pdatas          = V.map fromScalarPData datas

        -- Sum up each physical segment individually.
        !psegResults     = U.fold_ss (+) 0 (U.takeSSegdOfVSegd vsegd) pdatas

        -- Replicate the physical results according to the vsegids.
        !vsegResults     = U.bpermute psegResults (U.takeVSegidsOfVSegd vsegd) 

   in   PDouble vsegResults



-- Int ------------------------------------------------------------------------
{-# INLINE_PA sumPA_int #-}
sumPA_int   :: PArray Int  -> Int
sumPA_int (PArray _ (PInt xs))
        = U.sum xs


sumPA_l_int
        :: Int
        -> PData (PArray Int)
        -> PData Int
{-# NOINLINE sumPA_l_int #-}
sumPA_l_int _ (PNested vsegd datas)
 = {-# SCC "sumPA_l_int" #-}
   let  -- Grab all the flat source vectors.
        pdatas          = V.map fromScalarPData datas

        -- Sum up each physical segment individually.
        psegResults     = U.fold_ss (+) 0 (U.takeSSegdOfVSegd vsegd) pdatas

        -- Replicate the physical results according to the vsegids.
        vsegResults     = U.bpermute psegResults (U.takeVSegidsOfVSegd vsegd) 

   in   PInt vsegResults
