module Map_Map_Replicate where
import Data.Array.Parallel.Unlifted

-- > 1 loopU/loopU
-- > 1 loopU/replicateU

map_map_replicate :: (UA a, UA b, UA c)
                  => (b -> c) -> (a -> b) -> Int -> a -> UArr c
map_map_replicate f g n = mapU f . mapU g . replicateU n

