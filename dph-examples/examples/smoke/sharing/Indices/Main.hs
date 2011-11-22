
-- | This divide and conquer program accesseses top-level array from the
--   computations at the bottom of the tree. In lifted backends that
--   don't manage sharing properly, this program will blow up when it tries
--   to replicate the top-level a array at every step in the division phase.
import Util
import Timing
import Randomish
import System.Environment
import Control.Exception
import qualified Vectorised                     as ID
import qualified Data.Array.Parallel.PArray     as P
import qualified Data.Vector.Unboxed            as V

main
 = do   args    <- getArgs
        
        case args of
         [alg, count] -> run alg (read count)
         _            -> usage


run "vectorised" count
 = do   let arr = P.fromList [0 .. count - 1]
        arr `seq` return ()     
                
        (arrResult, tElapsed)
         <- time
         $  let  arr'    = ID.indicesPA arr arr
            in   P.nf arr' `seq` return arr'

        print   $ P.length arrResult
        putStr  $ prettyTime tElapsed

run _ _
 = usage


usage   = putStr $ unlines
        [ "usage: indices <algorithm> <count>\n"
        , "  algorithm one of " ++ show ["vectorised"]
        , ""]
