import qualified Types as QH
import QuickHullVect (quickhull)

import Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.PArray as P

import Prelude as Prel
import qualified System.Random as R
import Control.Exception (evaluate)

import Bench.Benchmark
import Bench.Options


-- Random points generation
--

-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.

generatePoints :: Int -> IO (Point (PArray QH.Point))
generatePoints n
  = do
      let rg  = R.mkStdGen 42742     -- always use the same seed
          ps  = toPairs (take (2*n) (R.randomRs (-100, 100) rg))
          pts = QH.points (P.fromList (Prel.map fst ps))
                          (P.fromList (Prel.map snd ps))
      evaluate $ force pts
      return $ ("N = " ++ show n) `mkPoint` pts
  where
    toPairs []        = []
    toPairs (x:y:pts) = (x, y) : toPairs pts

    force pts = toUArrPA (QH.xsOf pts) U.!: 0 D.+ 
                toUArrPA (QH.ysOf pts) U.!: 0


-- Main
-- ----

{- Simple test
pts = points (P.fromList (Prel.map fst coords))
             (P.fromList (Prel.map snd coords))
  where
    coords = [(3,3),(2,7),(0,0),(8,5), (4,6),(5,3),(9,6),(10,0)]

result = Prel.zip (U.toList (toUArrPA (xsOf ps)))
                  (U.toList (toUArrPA (ysOf ps)))
  where
    ps = quickhull pts

main = print result
 -}

main = ndpMain "Quick hull"
               "[OPTION] ... SIZES ..."
               run [] ()

run opts () sizes =
  case Prel.map read sizes of
    []  -> failWith ["No sizes specified"]
    szs -> do
             benchmark opts runQuickhull
                (Prel.map generatePoints szs)
                (`seq` ()) (("Result length = " ++) . show)
             return ()
  where
    runQuickhull :: PArray QH.Point -> Int
    runQuickhull pts = let result = quickhull pts
                           resxs  = toUArrPA (QH.xsOf result)
                       in
                       resxs U.!: 0 `seq` U.length resxs
          
