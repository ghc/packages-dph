
module Data.Array.Parallel.PArray.PData.Wrap where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base

newtype instance PData (Wrap a)
        = PWrap (PData a)


instance PA a => PR (Wrap a) where
        