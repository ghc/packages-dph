#include "fusion-phases.h"

-- | Instances for the PRRepr/PA family and class.
--
--   This module is kept separate from PRepr.Base to break an import cycle
--   between PRepr.Base PRepr.Instances and PArray.PData.Wrap
--
module Data.Array.Parallel.PArray.PRepr.Instances where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Void
import Data.Array.Parallel.PArray.PData.Wrap
import Data.Array.Parallel.PArray.PData.Unit
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Sum2
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Int
import Data.Array.Parallel.PArray.PData.Double
import qualified Data.Array.Parallel.Unlifted   as U

-- Void -----------------------------------------------------------------------
type instance PRepr Void = Void

instance PA Void where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  toNestedArrPRepr      = id


-- Unit -----------------------------------------------------------------------
type instance PRepr () = ()

instance PA () where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  toNestedArrPRepr      = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int = Int

instance PA Int where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  toNestedArrPRepr      = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double = Double

instance PA Double where
  toPRepr               = id
  fromPRepr             = id
  toArrPRepr            = id
  fromArrPRepr          = id
  toArrPReprs           = id
  toNestedArrPRepr      = id
  
  
-- Bool -----------------------------------------------------------------------
data instance PData Bool
  = PBool U.Sel2

type instance PRepr Bool
  = Sum2 Void Void

instance PA Bool where
  {-# INLINE toPRepr #-}
  toPRepr False          = Alt2_1 void
  toPRepr True           = Alt2_2 void

  {-# INLINE fromPRepr #-}
  fromPRepr (Alt2_1 _)   = False
  fromPRepr (Alt2_2 _)   = True

  {-# INLINE toArrPRepr #-}
  toArrPRepr (PBool sel) = PSum2 sel pvoid pvoid

  {-# INLINE fromArrPRepr #-}
  fromArrPRepr (PSum2 sel _ _)
   = PBool sel

  