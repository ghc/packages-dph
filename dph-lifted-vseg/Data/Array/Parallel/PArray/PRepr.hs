#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PRepr
        ( module Data.Array.Parallel.PArray.PRepr.Base
        , module Data.Array.Parallel.PArray.PRepr.Instances
        , module Data.Array.Parallel.PArray.PRepr.Nested
        , module Data.Array.Parallel.PArray.PRepr.Tuple
        , packByTagsPA
        , mapdPA
        , zipWithdPA)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PRepr.Instances
import Data.Array.Parallel.PArray.PRepr.Nested
import Data.Array.Parallel.PArray.PRepr.Tuple
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U


-- | Filter some scattered segments according to some tag arrays.
--   The `SSegd` describes the layout of the source data as well as the tags,
--   which must be the same.
packByTagsPA
        :: PA a
        => U.SSegd
        -> PDatas a             -- ^ Source array
        -> PDatas Tag           -- ^ Tag arrays
        -> Tag                  -- ^ Tag of elements to select.
        -> (U.Segd, PData a)

packByTagsPA ssegd xdatas bdatas tag
 = let
        -- Gather the scattered data together into contiguous arrays, 
        -- which is the form packByTag needs.
        xdata_contig            = extractsPA xdatas ssegd
        bdata'@(PInt tags)      = extractsPA bdatas ssegd
         
        -- Pack all the psegs.
        xdata'          = packByTagPA xdata_contig tags 1

        -- Rebuild the segd to account for the possibly smaller segments.
        segd            = U.lengthsToSegd $ U.lengthsSSegd ssegd
  in    (segd, xdata')
{-# INLINE_PA packByTagsPA #-}


mapdPA  :: (PA a, PA b)
        => (PData  a -> PData  b) 
        ->  PDatas a -> PDatas b
mapdPA f xs
 = fromArrPReprs
 $ mapdPR
        (\x -> toArrPRepr $ f $ fromArrPRepr x)
        (toArrPReprs xs)
{-# INLINE_PA mapdPA #-}



zipWithdPA
        :: (PA a, PA b, PA c)
        => (PData  a -> PData  b -> PData  c)
        ->  PDatas a -> PDatas b -> PDatas c
zipWithdPA f xs ys
 = fromArrPReprs
 $ zipWithdPR
        (\x y -> toArrPRepr $ f (fromArrPRepr x) (fromArrPRepr y))
        (toArrPReprs xs) (toArrPReprs ys)
{-# INLINE_PA zipWithdPA #-}

