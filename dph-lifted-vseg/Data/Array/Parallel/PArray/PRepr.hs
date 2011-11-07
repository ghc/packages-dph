{-# LANGUAGE UndecidableInstances #-}
#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PRepr
        ( module Data.Array.Parallel.PArray.PRepr.Base
        , module Data.Array.Parallel.PArray.PRepr.Instances
        , module Data.Array.Parallel.PArray.PRepr.Nested
        , module Data.Array.Parallel.PArray.PRepr.Tuple
        , unpackPA
        , packByTagsPA
        , mapdPA
        , zipWithdPA)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PRepr.Instances
import Data.Array.Parallel.PArray.PRepr.Nested
import Data.Array.Parallel.PArray.PRepr.Tuple
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import GHC.Exts

-- Pretty -------------------------------------------------------------------
-- | Show a virtual array.
instance (Show a, PA a)
        => Show (PArray a) where
 show (PArray _ pdata)
  =  render 
        $ brackets 
        $ text "|"
                <> (hcat $ punctuate comma $ map (text . show) $ V.toList $ toVectorPA pdata)
                <> text "|"


-- | Pretty print a virtual array.
instance  (PprVirtual a, PA a)
        => PprVirtual (PArray a) where
 pprv (PArray _ pdata)
  =     brackets 
        $ text "|"
                <> (hcat $ punctuate comma $ map pprv $ V.toList $ toVectorPA pdata)
                <> text "|"


-- | To pretty print a physical PArray we need to print the elements in their
--   generic representations.
instance  (PprPhysical (PData (PRepr a)), PA a)
        => PprPhysical (PArray a) where
 pprp (PArray n# dat)
  =   (text "PArray " <+> int (I# n#))
  $+$ ( nest 4 
      $ pprp $ toArrPRepr dat)


-- | Pretty print the physical representation of a nested array
instance (PprPhysical (PData a), PR a) 
       => PprPhysical (PData (PArray a)) where
 pprp (PNested uvsegd pdatas)
  =   text "PNested"
  $+$ (nest 4 $ pprp uvsegd $$ (pprp $ pdatas))

{-
-- | Pretty print a virtual nested array.
instance ( PprVirtual (PData a), PR a) 
        => PprVirtual (PData (PArray a)) where
 pprv arr
  =   lbrack 
        <> hcat (punctuate comma 
                        $ map pprv 
                        $ V.toList $ toVectorPR arr)
   <> rbrack
-}

-- Unpack ----------------------------------------------------------------------
-- | Unpack an array to reveal its representation.
unpackPA :: PA a => PArray a -> PData (PRepr a)
unpackPA (PArray _ pdata)
        = toArrPRepr pdata
                
--------------------------------------------------------------------------------
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

