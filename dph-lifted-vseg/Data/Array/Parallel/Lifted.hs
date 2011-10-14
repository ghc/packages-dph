
module Data.Array.Parallel.Lifted ( 
        -- * Closures
        (:->)(..),
        ($:), liftedApply,
        
        -- * Constructors
        emptyPP,
        singletonPP,
        replicatePP,

        -- * Projections
        lengthPP,
        indexPP,
        slicePP,

        -- * Traversals
        mapPP,

        -- * Filtering
        filterPP,

        -- * Zipping and Unzipping
        unzipPP,

        -- * Other combinators
        appendPP,
        concatPP,

        -- * Scalar Functions
        plusPP_int,
        divPP_int,
        multPP_double,
        sumPP_double, sumPP_int
)
where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators



        
        
        
        
