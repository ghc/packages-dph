
module Data.Array.Parallel.PArray.Stream where

import Data.Array.Parallel.Stream
import Data.Array.Parallel.PArray.PData
import qualified Data.Vector                            as V
import qualified Data.Vector.Generic                    as G
import qualified Data.Vector.Fusion.Stream              as S
import qualified Data.Vector.Fusion.Stream.Size         as S
import qualified Data.Vector.Fusion.Stream.Monadic      as M
import qualified Data.Array.Parallel.Unlifted           as U
import qualified Data.Array.Parallel.Unlifted.Sequential.Vector as SU



-- | Lifted sum for Doubles
sumPA_l_double :: Int
        -> PData (PArray Double)
        -> PData Double
        
sumPA_l_double _ arr
 = let  psegResults
         = G.unstream 
         $ foldSS (+) 0
                (SU.stream (pnested_pseglens arr))
                (streamPSegsOfNested arr)
        
        vsegResults
         = U.bpermute psegResults (pnested_vsegids arr) 
                        
   in   PDouble vsegResults


-- | Lifted sum for Ints
sumPA_l_int :: Int
        -> PData (PArray Int)
        -> PData Int
        
sumPA_l_int _ arr
 = let  psegResults
         = G.unstream 
         $ foldSS (+) 0
                (SU.stream (pnested_pseglens arr))
                (streamPSegsOfNested arr)

        vsegResults
         = U.bpermute psegResults (pnested_vsegids arr) 
                        
   in   PInt vsegResults


-- | Stream all the psegs from an array.
streamPSegsOfNested :: PR a => PData (PArray a) -> S.Stream a
streamPSegsOfNested arr
        = streamPSegs 
                (pnested_pseglens   arr)
                (pnested_psegstarts arr)
                (pnested_psegsrcs   arr)
                (pnested_psegdata   arr)        


-- | Stream some physical segments from many data arrays.
--   TODO: make this more efficient, and fix fusion.
--         We should be able to eliminate a lot of the indexing happening in the 
--         inner loop by being cleverer about the loop state.
streamPSegs 
        :: PR a
        => U.Array Int          -- ^ length of segments
        -> U.Array Int          -- ^ starting index of segment
        -> U.Array Int          -- ^ source id of segment
        -> V.Vector (PData a)   -- ^ data arrays
        -> S.Stream a
        
streamPSegs pseglens psegstarts psegsrcs psegdata
 = let  
        -- We've finished streaming this pseg
        fn (pseg, ix)
         -- All psegs are done.
         | pseg >= U.length pseglens
         = return $ S.Done
         
         -- Current pseg is done
         | ix   >= pseglens U.!: pseg 
         = fn (pseg + 1, 0)

         -- Stream an element from this pseg
         | otherwise
         = let  srcid   = psegsrcs   U.!: pseg
                pdata   = psegdata   V.!  srcid
                start   = psegstarts U.!: pseg
                result  = pdata `indexPR` (start + ix)
           in   return $ S.Yield result
                                 (pseg, ix + 1)

   in   M.Stream fn (0, 0) S.Unknown
