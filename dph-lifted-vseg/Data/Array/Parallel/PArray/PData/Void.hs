
module Data.Array.Parallel.PArray.PData.Void where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base

-- | The Void type is used when representing enumerations. 
--   A type like Bool is represented as @Sum2 Void Void@, meaning that we only
--   only care about the tag of the data constructor and not its argumnent.
--
data instance PData Void

pvoid :: PData Void
pvoid =  error "Data.Array.Parallel.PData Void"

instance PR Void where
        