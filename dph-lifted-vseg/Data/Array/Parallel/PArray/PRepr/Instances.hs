#include "fusion-phases.h"
{-# LANGUAGE UndecidableInstances #-}
-- | Instances for the PRRepr/PA family and class.
--
--   This module is kept separate from PRepr.Base to break an import cycle
--   between PRepr.Base PRepr.Instances and PArray.PData.Wrap
--
module Data.Array.Parallel.PArray.PRepr.Instances where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Wrap
import Data.Array.Parallel.PArray.PData.Unit
import Data.Array.Parallel.PArray.PData.Void
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Sum2
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Int
import Data.Array.Parallel.PArray.PData.Double
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Text.PrettyPrint


-- Void -----------------------------------------------------------------------
type instance PRepr Void = Void

instance PA Void where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id
  toNestedArrPRepr      = id

-- Unit -----------------------------------------------------------------------
type instance PRepr () = ()

instance PA () where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id
  toNestedArrPRepr      = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int = Int

instance PA Int where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id
  toNestedArrPRepr      = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double = Double

instance PA Double where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  fromArrPReprs         = id
  toNestedArrPRepr      = id
  
  
-- Bool -----------------------------------------------------------------------
-- | We use the `Void` type for both sides because we only care about the tag.
--   The `Void` fields don't use any space at runtime.
type instance PRepr Bool
  = Sum2 Void Void

data instance PData Bool
  = PBool U.Sel2

data instance PDatas Bool
  = PBools (V.Vector U.Sel2) (PDatas Tag)

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False          = Alt2_1 void
  toPRepr True           = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _)   = False
  fromPRepr (Alt2_2 _)   = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) 
        = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _)
        = PBool sel

  {-# INLINE toArrPReprs #-}
  toArrPReprs (PBools sels tagss)
        = PSum2s sels tagss 
                (pvoids $ V.length sels)
                (pvoids $ V.length sels)

  {-# INLINE fromArrPReprs #-}
  fromArrPReprs (PSum2s sels tagss _ _)
        = PBools sels tagss


-- Either ---------------------------------------------------------------------
type instance PRepr (Either a b)
 = Sum2 a b
 
data instance PData (Either a b)
 = PEither U.Sel2 (PData a) (PData b)

data instance PDatas (Either a b)
 = PEithers (V.Vector U.Sel2) (PDatas Tag) (PDatas a) (PDatas b)

instance (PR a, PR b) => PA (Either a b) where
  {-# INLINE toPRepr #-}
  toPRepr xx
   = case xx of
        Left x    -> Alt2_1 x
        Right y   -> Alt2_2 y

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 x)   = Left x
  fromPRepr (Alt2_2 x)   = Right x

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PEither sel pdata1 pdata2)
        = PSum2 sel pdata1 pdata2
        
  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel pdata1 pdata2)
        = PEither sel pdata1 pdata2

  {-# INLINE toArrPReprs #-}
  toArrPReprs (PEithers sels tags pdatas1 pdatas2)
        = PSum2s sels tags pdatas1 pdatas2

  {-# INLINE fromArrPReprs #-}
  fromArrPReprs (PSum2s sels tags pdatas1 pdatas2)
        = PEithers sels tags pdatas1 pdatas2

{-
instance ( PprPhysical (PData a), PR a, Eq a
         , PprPhysical (PData b), PR b, Eq b)
        => PprPhysical (PData (Either a b)) where
 pprp xs = pprp $ toArrPRepr xs

-}
