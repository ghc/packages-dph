module Map_Replicate where
import Data.Array.Parallel.Unlifted

-- > 1 loopU/loopU

map_replicate :: (UA a, UA b) => (a -> b) -> Int -> a -> UArr b
map_replicate f n = mapU f . replicateU n

