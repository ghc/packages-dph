import Types
import QH

import Data.Array.Parallel.Lifted
import Data.Array.Parallel.Unlifted

pts = points (fromUArrPA' (toU (map fst coords)))
             (fromUArrPA' (toU (map snd coords)))
  where
    coords = [(3,3),(2,7),(0,0),(8,5), (4,6),(5,3),(9,6),(10,0)]

result = zip (fromU (toUArrPA (xsOf ps)))
             (fromU (toUArrPA (ysOf ps)))
  where
    ps = quickHull pts

main = print result

