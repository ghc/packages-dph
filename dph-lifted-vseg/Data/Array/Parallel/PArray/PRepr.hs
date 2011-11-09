{-# LANGUAGE UndecidableInstances #-}
#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PRepr
        ( module Data.Array.Parallel.PArray.PRepr.Base
        , module Data.Array.Parallel.PArray.PRepr.Instances
        , module Data.Array.Parallel.PArray.PRepr.Nested
        , module Data.Array.Parallel.PArray.PRepr.Tuple
        , unpackPA
        , mapdPA
        , zipWithdPA)
where
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PRepr.Instances
import Data.Array.Parallel.PArray.PRepr.Nested
import Data.Array.Parallel.PArray.PRepr.Tuple
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.Pretty
import Data.Vector                              (Vector)
import qualified Data.Vector                    as V


-- Pretty -------------------------------------------------------------------
-- | Show a virtual array.
instance (Show a, PA a)
        => Show (PArray a) where
 show (PArray _ pdata)
        = render 
        $ brackets 
        $ text "|"
                <> (hcat $ punctuate comma $ map (text . show) $ V.toList $ toVectorPA pdata)
                <> text "|"


-- | Pretty print a virtual array.
instance  (PprVirtual a, PA a)
        => PprVirtual (PArray a) where
 pprv (PArray _ pdata)
        = brackets 
        $ text "|"
                <> (hcat $ punctuate comma $ map pprv $ V.toList $ toVectorPA pdata)
                <> text "|"


instance PA a => PprPhysical (Vector a) where
 pprp vec
        = brackets 
        $ hcat
        $ punctuate (text ", ") 
        $ V.toList $ V.map pprpPA vec


-- Unpack ----------------------------------------------------------------------
-- | Unpack an array to reveal its representation.
unpackPA :: PA a => PArray a -> PData (PRepr a)
unpackPA (PArray _ pdata)
        = toArrPRepr pdata


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

