import Types
import QuickHullVect (quickhull)

import Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.PArray as P

import Prelude as Prel

pts = points (P.fromList (Prel.map fst coords))
             (P.fromList (Prel.map snd coords))
  where
    coords = [(3,3),(2,7),(0,0),(8,5), (4,6),(5,3),(9,6),(10,0)]

result = Prel.zip (U.toList (toUArrPA (xsOf ps)))
                  (U.toList (toUArrPA (ysOf ps)))
  where
    ps = quickhull pts

main = print result

