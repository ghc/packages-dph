module DotP where
import Data.Array.Parallel.Unlifted

-- > 1 loopU/loopU

dotp :: UArr Double -> UArr Double -> Double
dotp v w = sumU (zipWithU (*) v w)

