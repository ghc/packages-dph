import Data.Array.Parallel.Unlifted

import Bench.Benchmark
import Bench.Options

import System.Random
import System.Console.GetOpt

import Pipelines as P

type Gen a = forall g. RandomGen g => Int -> g -> IO a

data Algo = forall a b. Algo (a -> b) (Gen a)

algs :: [(String, Algo)]
algs = [("pipe1", Algo (uncurry pipe1) (uarr >< uarr))
       ,("pipe2", Algo pipe2           uarr)
       ,("pipe3", Algo pipe3           uarr)
       ,("pipe4", Algo pipe4           suarr)
       ,("pipe5", Algo pipe5           uarr)
       ]

uarr :: (UA a, Random a) => Gen (UArr a)
uarr n g = return $! randomU n g

suarr :: (UA a, Random a) => Gen (SUArr a)
suarr n g =
  do let lens = replicateU (n `div` 10) (10 :: Int)
         segd = lengthsToUSegd lens
         n'   = (n `div` 10) * 10
         arr  = randomU n' g
     segd `seq` arr `seq` return (segd >: arr)
            
(><) :: Gen a -> Gen b -> Gen (a,b)
(h1 >< h2) n g = let (g1,g2) = split g
                 in
                 do x <- h1 n g1
                    y <- h2 n g2
                    return (x,y)

randomGens :: RandomGen g => Int -> g -> [g]
randomGens 0 g = []
randomGens n g = let (g1,g2) = split g
                 in g1 : randomGens (n-1) g2

main = ndpMain "SpecConstr test"
               "[OPTION] ... SIZE"
               run [Option ['a'] ["algo"] (ReqArg const "ALGORITHM")
                      "use the selected algorithm"]
                   "<none>"

run opts alg sizes =
  case lookup alg algs of
    Nothing      -> failWith ["Unknown algorithm"]
    Just (Algo f gen) ->
      case map read sizes of
        []  -> failWith ["No sizes specified"]
        szs -> do
                 g <- getStdGen
                 let gs = randomGens (length szs) g
                 benchmark opts f
                   (zipWith (mk gen) szs gs)
                   (const "")
                 return ()
  where
    mk gen n g = do
                   x <- gen n g
                   return $ ("N = " ++ show n) `mkPoint` x

