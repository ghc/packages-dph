{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Types.USSegd (
        lengthUSSegdD,
        lengthsUSSegdD,
        indicesUSSegdD,
        elementsUSSegdD,
        startsUSSegdD,
        sourcesUSSegdD,
        usegdUSSegdD
) where
import Data.Array.Parallel.Unlifted.Distributed.Types.USegd
import Data.Array.Parallel.Unlifted.Distributed.Types.Vector
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Data.Array.Parallel.Unlifted.Sequential.USSegd
import Data.Array.Parallel.Unlifted.Sequential.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Data.Array.Parallel.Pretty
import Control.Monad
import Prelude                          as P


instance DT USSegd where
  data Dist USSegd   
        = DUSSegd  !(Dist (Vector Int))         -- segment starts
                   !(Dist (Vector Int))         -- segment sources
                   !(Dist USegd)                -- distributed usegd

  data MDist USSegd s 
        = MDUSSegd !(MDist (Vector Int) s)      -- segment starts
                   !(MDist (Vector Int) s)      -- segment sources
                   !(MDist USegd        s)      -- distributed usegd

  indexD (DUSSegd starts sources usegds) i
   = mkUSSegd (indexD starts i) (indexD sources i) (indexD usegds i)

  newMD g
   = liftM3 MDUSSegd (newMD g) (newMD g) (newMD g)

  readMD (MDUSSegd starts sources usegds) i
   = liftM3 mkUSSegd (readMD starts i) (readMD sources i) (readMD usegds i)

  writeMD (MDUSSegd starts sources usegds) i ussegd
   = do writeMD starts  i (startsUSSegd  ussegd)
        writeMD sources i (sourcesUSSegd ussegd)
        writeMD usegds  i (usegdUSSegd   ussegd)

  unsafeFreezeMD (MDUSSegd starts sources usegds)
   = liftM3 DUSSegd (unsafeFreezeMD starts)
                    (unsafeFreezeMD sources)
                    (unsafeFreezeMD usegds)

  deepSeqD ussegd z
   = deepSeqD (startsUSSegd  ussegd)
   $ deepSeqD (sourcesUSSegd ussegd)
   $ deepSeqD (usegdUSSegd   ussegd) z

  sizeD  (DUSSegd  _ _ usegd) = sizeD usegd
  sizeMD (MDUSSegd _ _ usegd) = sizeMD usegd

  measureD ussegd 
   = "USSegd "  P.++ show (startsUSSegd    ussegd)
   P.++ " "     P.++ show (sourcesUSSegd   ussegd)
   P.++ " "     P.++ measureD (usegdUSSegd ussegd)


instance PprPhysical (Dist USSegd) where
 pprp (DUSSegd starts sources usegds)
  =  text "DUSSegd"
  $$ (nest 7 $ vcat
        [ text "starts:  " <+> pprp starts
        , text "sources: " <+> pprp sources
        , text "usegds:  " <+> pprp usegds])


-- | O(1). Yield the overall number of segments.
lengthUSSegdD :: Dist USSegd -> Dist Int
{-# INLINE_DIST lengthUSSegdD #-}
lengthUSSegdD (DUSSegd starts _ _) 
        = lengthD starts


-- | O(1). Yield the lengths of the individual segments.
lengthsUSSegdD :: Dist USSegd -> Dist (Vector Int)
{-# INLINE_DIST lengthsUSSegdD #-}
lengthsUSSegdD (DUSSegd _ _ usegds)
        = lengthsUSegdD usegds


-- | O(1). Yield the segment indices.
indicesUSSegdD :: Dist USSegd -> Dist (Vector Int)
{-# INLINE_DIST indicesUSSegdD #-}
indicesUSSegdD (DUSSegd _ _ usegds)
        = indicesUSegdD usegds


-- | O(1). Yield the number of data elements.
elementsUSSegdD :: Dist USSegd -> Dist Int
{-# INLINE_DIST elementsUSSegdD #-}
elementsUSSegdD (DUSSegd _ _ usegds)
        = elementsUSegdD usegds


-- | O(1). Yield the starting indices.
startsUSSegdD :: Dist USSegd -> Dist (Vector Int)
{-# INLINE_DIST startsUSSegdD #-}
startsUSSegdD (DUSSegd starts _ _)
        = starts
        
-- | O(1). Yield the source ids
sourcesUSSegdD :: Dist USSegd -> Dist (Vector Int)
{-# INLINE_DIST sourcesUSSegdD #-}
sourcesUSSegdD (DUSSegd _ sources _)
        = sources


-- | O(1). Yield the USegd
usegdUSSegdD :: Dist USSegd -> Dist USegd
{-# INLINE_DIST usegdUSSegdD #-}
usegdUSSegdD (DUSSegd _ _ usegd)
        = usegd

