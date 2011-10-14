
module Data.Array.Parallel.PArray.Scalar 
        ( Scalar(..) )
where
import Data.Array.Parallel.PArray.PData
import qualified Data.Array.Parallel.Unlifted   as U


class U.Elt a => Scalar a where
  fromScalarPData :: PData a -> U.Array a
  toScalarPData   :: U.Array a -> PData a


instance Scalar Int where
  fromScalarPData (PInt xs)     = xs
  toScalarPData                 = PInt

instance Scalar Double where
  fromScalarPData (PDouble xs)  = xs
  toScalarPData                 = PDouble
