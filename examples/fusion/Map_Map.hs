module Map_Map where
import Data.Array.Parallel.Unlifted

-- > 1 loopU/loopU

map_map :: (Int -> Int) -> (Int -> Int) -> UArr Int -> UArr Int
map_map f g = mapU f . mapU g

