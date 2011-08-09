
-- | Parallel array types and primitive operators.
module Data.Array.Parallel.PArray.PData 
        ( module Data.Array.Parallel.PArray.PData.Base
        , module Data.Array.Parallel.PArray.PData.Scalar
        , module Data.Array.Parallel.PArray.PData.Tuple
        , module Data.Array.Parallel.PArray.PData.Nested
        , module Data.Array.Parallel.PArray.PData.Closure
        
          -- * Derived, polymorphic operators.
        , replicatePR)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Closure

import qualified Data.Array.Parallel.Unlifted	as U


-- Derived polymorphic operators ----------------------------------------------
{-# INLINE_PDATA replicatePR #-}
replicatePR :: PR a => Int -> a -> PData Sized a
replicatePR n x = restrictPJ n (repeatPE x)


