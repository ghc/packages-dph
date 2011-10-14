
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
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Int
import Data.Array.Parallel.PArray.PData.Double


-- Unit -----------------------------------------------------------------------
type instance PRepr () = ()

instance PA () where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Int ------------------------------------------------------------------------
type instance PRepr Int = Int

instance PA Int where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id


-- Double ---------------------------------------------------------------------
type instance PRepr Double = Double

instance PA Double where
  toPRepr      = id
  fromPRepr    = id
  toArrPRepr   = id
  fromArrPRepr = id
  
  