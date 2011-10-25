
-- | Parallel array types and primitive operators.
module Data.Array.Parallel.PArray.PData 
        ( module Data.Array.Parallel.PArray.PData.Base
        , module Data.Array.Parallel.PArray.PData.Wrap
        , module Data.Array.Parallel.PArray.PData.Double
        , module Data.Array.Parallel.PArray.PData.Nested
        , module Data.Array.Parallel.PArray.PData.Unit
        , module Data.Array.Parallel.PArray.PData.Tuple
        , module Data.Array.Parallel.PArray.PData.Void
        , mapdPR
        , zipWithdPR)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Wrap
import Data.Array.Parallel.PArray.PData.Int             ()
import Data.Array.Parallel.PArray.PData.Double
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Sum2
import Data.Array.Parallel.PArray.PData.Unit
import Data.Array.Parallel.PArray.PData.Tuple
import Data.Array.Parallel.PArray.PData.Void
import Data.Array.Parallel.PArray.PRepr.Instances
import Data.Array.Parallel.Base                 (Tag)
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V



-- | Apply a worked function to all PData in a collection.
mapdPR  :: (PR a, PR b)
        => (PData a -> PData b)
        -> PDatas a -> PDatas b
mapdPR f pdatas
        = fromVectordPR $ V.map f (toVectordPR pdatas)


-- | Combine all PData in a collection with an operator.
zipWithdPR
        :: (PR a, PR b, PR c)
        => (PData a -> PData b  -> PData c)
        -> PDatas a -> PDatas b -> PDatas c
zipWithdPR f pdatas1 pdatas2
        = fromVectordPR $ V.zipWith f 
                (toVectordPR pdatas1)
                (toVectordPR pdatas2)

