{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of Virtual Segment Descriptors
module Data.Array.Parallel.Unlifted.Distributed.Types.UVSegd (
        lengthD,
        takeLengthsD,
        takeIndicesD,
        takeElementsD,
        takeStartsD,
        takeSourcesD,
        takeVSegidsD,
        takeUSSegdD
) where
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Data.Array.Parallel.Unlifted.Sequential.UVSegd                   (UVSegd)
import Data.Array.Parallel.Unlifted.Sequential.USSegd                   (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.Vector
import Data.Array.Parallel.Pretty
import Control.Monad
import Prelude                          as P

import qualified Data.Array.Parallel.Unlifted.Sequential.UVSegd         as UVSegd
import qualified Data.Array.Parallel.Unlifted.Distributed.Types.USSegd  as DUSegd


instance DT UVSegd where
  data Dist UVSegd   
        = DUVSegd  !(Dist (Vector Int))         -- vsegids
                   !(Dist USSegd)               -- distributed ussegd

  data MDist UVSegd s 
        = MDUVSegd !(MDist (Vector Int) s)      -- vsegids
                   !(MDist USSegd       s)      -- distributed ussegd

  indexD (DUVSegd vsegids ussegds) i
   = UVSegd.mkUVSegd (indexD vsegids i) (indexD ussegds i)

  newMD g
   = liftM2 MDUVSegd (newMD g) (newMD g)

  readMD (MDUVSegd vsegids ussegds) i
   = liftM2 UVSegd.mkUVSegd (readMD vsegids i) (readMD ussegds i)

  writeMD (MDUVSegd vsegids ussegds) i uvsegd
   = do writeMD vsegids  i (UVSegd.takeVSegids  uvsegd)
        writeMD ussegds  i (UVSegd.takeUSSegd   uvsegd)

  unsafeFreezeMD (MDUVSegd vsegids ussegds)
   = liftM2 DUVSegd (unsafeFreezeMD vsegids)
                    (unsafeFreezeMD ussegds)

  deepSeqD uvsegd z
   = deepSeqD (UVSegd.takeVSegids  uvsegd)
   $ deepSeqD (UVSegd.takeUSSegd   uvsegd) z

  sizeD  (DUVSegd  _ ussegd) = sizeD ussegd
  sizeMD (MDUVSegd _ ussegd) = sizeMD ussegd

  measureD uvsegd 
   = "UVSegd " P.++ show (UVSegd.takeVSegids    uvsegd)
   P.++ " "    P.++ measureD (UVSegd.takeUSSegd uvsegd)


instance PprPhysical (Dist UVSegd) where
 pprp (DUVSegd vsegids ussegds)
  =  text "DUVSegd"
  $$ (nest 7 $ vcat
        [ text "vsegids: " <+> pprp vsegids
        , text "ussegds: " <+> pprp ussegds])


-- | O(1). Yield the overall number of segments.
lengthD :: Dist UVSegd -> Dist Int
{-# INLINE_DIST lengthD #-}
lengthD (DUVSegd _ ussegd) 
        = DUSegd.lengthD ussegd


-- | O(1). Yield the lengths of the individual segments.
takeLengthsD :: Dist UVSegd -> Dist (Vector Int)
{-# INLINE_DIST takeLengthsD #-}
takeLengthsD (DUVSegd _ ussegd)
        = DUSegd.takeLengthsD ussegd


-- | O(1). Yield the segment indices.
takeIndicesD :: Dist UVSegd -> Dist (Vector Int)
{-# INLINE_DIST takeIndicesD #-}
takeIndicesD (DUVSegd _ ussegd)
        = DUSegd.takeIndicesD ussegd


-- | O(1). Yield the number of data elements.
takeElementsD :: Dist UVSegd -> Dist Int
{-# INLINE_DIST takeElementsD #-}
takeElementsD (DUVSegd _ ussegd)
        = DUSegd.takeElementsD ussegd


-- | O(1). Yield the starting indices.
takeStartsD :: Dist UVSegd -> Dist (Vector Int)
{-# INLINE_DIST takeStartsD #-}
takeStartsD (DUVSegd _ ussegd)
        = DUSegd.takeStartsD ussegd
        
        
-- | O(1). Yield the source ids
takeSourcesD :: Dist UVSegd -> Dist (Vector Int)
{-# INLINE_DIST takeSourcesD #-}
takeSourcesD (DUVSegd _ ussegd)
        = DUSegd.takeSourcesD ussegd


-- | O(1). Yield the vsegids
takeVSegidsD :: Dist UVSegd -> Dist (Vector Int)
{-# INLINE_DIST takeVSegidsD #-}
takeVSegidsD (DUVSegd vsegids _)
        = vsegids


-- | O(1). Yield the USSegd
takeUSSegdD :: Dist UVSegd -> Dist USSegd
{-# INLINE_DIST takeUSSegdD #-}
takeUSSegdD (DUVSegd _ ussegd)
        = ussegd
