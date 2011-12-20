
module Data.Array.Parallel.Unlifted.Stream
        ( -- * Index streamers.
          streamSrcIxsThroughVSegids
        , streamSrcIxsThroughUSSegd

          -- * Element streamers.
        , streamElemsFromVector
        , streamElemsFromVectors
        , streamElemsFromVectorsVSegd

          -- * Segment streamers.
        , streamSegsFromNestedUSSegd
        , streamSegsFromVectorsUSSegd
        , streamSegsFromVectorsUVSegd)
where
import Data.Array.Parallel.Unlifted.Stream.Ixs
import Data.Array.Parallel.Unlifted.Stream.Elems
import Data.Array.Parallel.Unlifted.Stream.Segments
