
module Test 
        ( module Data.Array.Parallel.Unlifted.Distributed
        , module Data.Array.Parallel.Unlifted.Sequential.Segmented
        , module Data.Array.Parallel.Pretty
        , module Data.Array.Parallel.Unlifted.Distributed.Types
        , module Data.Vector.Unboxed
        , usegd
        , ussegd)
where
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Sequential.Segmented
import Data.Array.Parallel.Pretty               hiding (empty)
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Vector.Unboxed
 

usegd   = lengthsToUSegd
                (fromList [80, 10, 20, 40, 50, 10])

ussegd  = mkUSSegd
                (fromList [100, 190, 200, 300, 450, 490])
                (fromList [0,   0,   1,   2,   2,   3])
                usegd