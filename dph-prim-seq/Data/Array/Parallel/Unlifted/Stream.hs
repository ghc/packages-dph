
module Data.Array.Parallel.Unlifted.Stream
        ( -- * Index streamers.
          unsafeStreamSrcIxsThroughVSegids
        , unsafeStreamSrcIxsThroughUSSegd

          -- * Element streamers.
        , unsafeStreamElemsFromVectors
        , unsafeStreamElemsFromVectorsVSegd

          -- * Segment streamers.
        , unsafeStreamSegsFromNestedUSSegd
        , unsafeStreamSegsFromVectorsUSSegd)
where
import Data.Array.Parallel.Unlifted.Stream.Ixs
import Data.Array.Parallel.Unlifted.Stream.Elems
import Data.Array.Parallel.Unlifted.Stream.Segments
