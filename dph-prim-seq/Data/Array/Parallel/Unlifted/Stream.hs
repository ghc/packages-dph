
module Data.Array.Parallel.Unlifted.Stream
        ( -- * Index streamers.
          streamSrcIxsThroughVSegids
        , streamSrcIxsThroughUSSegd

          -- * Element streamers.
        , streamElemsFromVectors
        , streamElemsFromVectorsVSegd

          -- * Segment streamers.
        , streamSegsFromNestedUSSegd
        , streamSegsFromVectorsUSSegd)
where
import Data.Array.Parallel.Unlifted.Stream.Ixs
import Data.Array.Parallel.Unlifted.Stream.Elems
import Data.Array.Parallel.Unlifted.Stream.Segments
