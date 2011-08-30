-- | Segment descriptors for virtual arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.UVSegd (
        -- * Types
        UVSegd(..),

        -- * Consistency check
        validUVSegd,
        
        -- * Constructors
        mkUVSegd,
        emptyUVSegd,
        singletonUVSegd,
        promoteUSegdToUVSegd,
        promoteUSSegdToUVSegd,
        
        -- * Projections
        vsegidsUVSegd,
        ussegdUVSegd,
        lengthUVSegd,
        lengthsUVSegd,
        getSegOfUVSegd,

        -- * Operators
        appendUVSegd,
        combine2UVSegd,
        updateVSegsOfUVSegd,
        demoteUVSegdToUSSegd,
        unsafeMaterializeUVSegd
) where
import Data.Array.Parallel.Unlifted.Sequential.USel
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USSegd
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import Data.Array.Parallel.Pretty


-- UVSegd ---------------------------------------------------------------------
-- TODO shift this to its own module
-- | Defines a virutal nested array based on physical segmentation.
--
--   Or alternatively: represents an index space transformation between
--   indices for the nested array and indices for the physical data.
--   
--   TODO: It'd probably be better to represent the vsegids as a lens (function)
--         instead of a vector of segids. Much of the time the vsegids are just [0..n] 
--
data UVSegd 
        = UVSegd 
        { uvsegd_vsegids :: !(Vector Int) 
          -- ^ array saying which physical segment to use for each virtual segment 

        , uvsegd_ussegd  :: !USSegd
          -- ^ slice segment descriptor describing physical segments.
        }
        deriving (Show)


-- | Pretty print the physical representation of a `UVSegd`
instance PprPhysical UVSegd where
 pprp (UVSegd vsegids ussegd)
  =   vcat
  [   text "UVSegd" $$ (nest 7 $ text "vsegids:" <+> (text $ show $ V.toList vsegids))
  ,   pprp ussegd ]


-- Constructors ---------------------------------------------------------------
-- | O(1). 
--   Construct a new slice segment descriptor.
--   All the provided arrays must have the same lengths.
mkUVSegd
        :: Vector Int   -- ^ array saying which physical segment to use for each
                        --   virtual segment.
        -> USSegd       -- ^ slice segment descriptor describing physical segments.
        -> UVSegd

{-# INLINE mkUVSegd #-}
mkUVSegd = UVSegd


-- | O(1).
--   Check the internal consistency of a virutal segmentation descriptor.
--   TODO: check that all vsegs point to a valid pseg
validUVSegd :: UVSegd -> Bool
{-# INLINE validUVSegd #-}
validUVSegd (UVSegd vsegids ussegd)
        = V.length vsegids == lengthUSSegd ussegd


-- | O(1).
--  Yield an empty segment descriptor, with no elements or segments.
emptyUVSegd :: UVSegd
{-# INLINE emptyUVSegd #-}
emptyUVSegd = UVSegd V.empty emptyUSSegd


-- | O(1).
--   Yield a singleton segment descriptor.
--   The single segment covers the given number of elements in a flat array
--   with sourceid 0.
singletonUVSegd :: Int -> UVSegd
{-# INLINE singletonUVSegd #-}
singletonUVSegd n 
        = UVSegd (V.singleton 0) (singletonUSSegd n)


-- | O(segs). 
--   Promote a plain USSegd to a UVSegd
--   The result contains one virtual segment for every physical segment
--   the provided USSegd.
--
promoteUSSegdToUVSegd :: USSegd -> UVSegd
{-# INLINE promoteUSSegdToUVSegd #-}
promoteUSSegdToUVSegd ussegd
        = UVSegd (V.enumFromTo 0 (lengthUSSegd ussegd - 1))
                 ussegd

-- | O(segs). 
--   Promote a plain USegd to a UVSegd
--   All segments are assumed to come from a flat array with sourceid 0.
--   The result contains one virtual segment for every physical segment
--   the provided USegd.
--
promoteUSegdToUVSegd :: USegd -> UVSegd
{-# INLINE promoteUSegdToUVSegd #-}
promoteUSegdToUVSegd
        = promoteUSSegdToUVSegd
        . promoteUSegdToUSSegd
        

-- Projections ----------------------------------------------------------------
vsegidsUVSegd :: UVSegd -> Vector Int
{-# INLINE vsegidsUVSegd #-}
vsegidsUVSegd (UVSegd vsegids _)
        = vsegids
        
ussegdUVSegd :: UVSegd -> USSegd
{-# INLINE ussegdUVSegd #-}
ussegdUVSegd (UVSegd _ ussegd)
        = ussegd

lengthUVSegd :: UVSegd -> Int
{-# INLINE lengthUVSegd #-}
lengthUVSegd (UVSegd vsegids _)
        = V.length vsegids

-- | O(segs).
--   Yield the lengths of the segments described by a `UVSegd`.
lengthsUVSegd :: UVSegd -> Vector Int
{-# INLINE lengthsUVSegd #-}
lengthsUVSegd (UVSegd vsegids ussegd)
        = V.map ((ussegd_lengths ussegd) V.!) vsegids


-- | O(1).
--  Get the length, starting index, and sourceid of a segment.
getSegOfUVSegd :: UVSegd -> Int -> (Int, Int, Int)
{-# INLINE getSegOfUVSegd #-}
getSegOfUVSegd (UVSegd vsegids ussegd) ix
        = getSegOfUSSegd ussegd (vsegids V.! ix)
        

-- Operators ------------------------------------------------------------------
-- | TODO: automatically force out unreachable psegs here.
updateVSegsOfUVSegd :: (Vector Int -> Vector Int) -> UVSegd -> UVSegd
{-# INLINE updateVSegsOfUVSegd #-}
updateVSegsOfUVSegd f (UVSegd vsegids ussegd)
 = let  (vsegids', ussegd') = cullUSSegdOnVSegids (f vsegids) ussegd
   in   UVSegd vsegids' ussegd'


-- | O(segs)
--   Yield a `USSegd` that describes each segment of a `UVSegd` individually.
-- 
--   * By doing this we lose information about virtual segments corresponding
--     to the same physical segments.
-- 
--   * This operation is used in concatPR as the first step in eliminating
--     segmentation from a nested array.
-- 
demoteUVSegdToUSSegd :: UVSegd -> USSegd
{-# INLINE demoteUVSegdToUSSegd #-}
demoteUVSegdToUSSegd (UVSegd vsegids ussegd)
        = mkUSSegd (V.bpermute (lengthsUSSegd ussegd) vsegids)
                   (V.bpermute (indicesUSSegd ussegd) vsegids)
                   (V.bpermute (sourcesUSSegd ussegd) vsegids)


-- | O(segs)
--   Given an virtual segment descriptor, produce a plain USegd that
--   that describes the entire array.
--
--   WARNING:
--   Trying to take the USegd of a nested array that has been constructed with
--   replication can cause index overflow. This is because the virtual size of
--   the corresponding flat data can be larger than physical memory.
-- 
--   You should only apply this function to a nested array when you're about
--   about to construct something with the same size as the corresponding
--   flat array. In this case the index overflow doesn't matter too much
--   because the program would OOM anyway.
--
unsafeMaterializeUVSegd :: UVSegd -> USegd
{-# INLINE unsafeMaterializeUVSegd #-}
unsafeMaterializeUVSegd (UVSegd vsegids ussegd)
        = lengthsToUSegd 
        $ V.bpermute (lengthsUSSegd ussegd) vsegids


-- append ---------------------------------------------------------------------
-- | O(n)
--   Produce a segment descriptor describing the result of appending two arrays.
--   Note that the implementation of this is similar to `combine2UVSegd`
--
--   source
--    VIRT1 [[0],[4,2],[5,6,7,8,9]]
--    PHYS1 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [1,2,5]
--                  psegstarts: [0,1,3]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [0,4,2,5,6,7,8,9]
--
--    VIRT2 [[1,2,3],[8,6,3],[9,3]]
--    PHYS2 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [3,3,2]
--                  psegstarts: [0,3,6]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [1,2,3,8,6,3,9,3]
--
---  appended
--    VIRT  [[0],[4,2],[5,6,7,8,9],[1,2,3],[8,6,3],[9,3]]
--          UVSegd  vsegids:    [0,1,2,3,4,5]  -- shift second half
--          USSegd  pseglens:   [1,2,5,3,3,2]  -- appended
--                  psegstarts: [0,1,3,0,3,6]  -- appended
--                  psegsrcs:   [0,0,0,1,1,1]  -- shift second half
--          PData   PInt [0,4,2,5,6,7,8,9]     -- both pdatas in result
--                  PInt [1,2,3,8,6,3,9,3]     -- ...
--
appendUVSegd
        :: UVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd -> Int  -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd

{-# INLINE appendUVSegd #-}
appendUVSegd (UVSegd vsegids1 ussegd1) pdatas1
             (UVSegd vsegids2 ussegd2) pdatas2

 = let  -- vsegids releative to appended psegs
        vsegids1' = vsegids1
        vsegids2' = V.map (+ lengthUSSegd ussegd1) vsegids2
        
        -- append the vsegids
        vsegids'  = vsegids1 V.++ vsegids2'

        -- All data from the source arrays goes into the result
        ussegd'   = appendUSSegd ussegd1 pdatas1
                                 ussegd2 pdatas2
                                 
   in   UVSegd vsegids' ussegd'


-- combine --------------------------------------------------------------------
-- | O(n)
--   Combine two virtual segment descriptors.
--   Note that the implementation of this is similar to `appendUVSegd`
--
--   source
--    VIRT1 [[0],[4,2],[5,6,7,8,9]]
--    PHYS1 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [1,2,5]
--                  psegstarts: [0,1,3]
--                  psegsrcs:   [0,0,0]
--          PDATA   PInt [0,4,2,5,6,7,8,9]
--
--    VIRT2 [[1,2,3],[8,6,3],[9,3]]
--    PHYS2 UVSegd  vsegids:    [0,1,2]
--          USSegd  pseglens:   [3,3,2]
--                  psegstarts: [0,3,6]
--                  psegsrcs:   [0,0,0]
--          PData   PInt [1,2,3,8,6,3,9,3]
--
--   combined with tags [1,0,0,1,0,1]
--    VIRT  [[1,2,3],[0],[4,2],[8,6,3],[5,6,7,8,9],[9,3]]
--    PHYS  VSSegd  vsegids:    [3,0,1,4,2,5] -- combine shifted vsegs
--          USSegd  pseglens:   [1,2,5,3,3,2] -- appended
--                  psegstarts: [0,1,3,0,3,6] -- appended
--                  psegsrcs:   [0,0,0,1,1,1] -- shift second half
--          PData   PInt [0,4,2,5,6,7,8,9]    -- both pdatas in result
--                  PInt [1,2,3,8,6,3,9,3]
--   
combine2UVSegd
        :: USel2
        -> UVSegd -> Int   -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd -> Int   -- ^ uvsegd of array, and number of physical data arrays
        -> UVSegd
        
{-# INLINE combine2UVSegd #-}
combine2UVSegd  usel2
                (UVSegd vsegids1 ussegd1) pdatas1
                (UVSegd vsegids2 ussegd2) pdatas2

 = let  -- vsegids relative to combined psegs
        vsegids1' = vsegids1
        vsegids2' = V.map (+ (V.length vsegids1)) vsegids2

        -- combine the vsegids
        vsegids'  = combine2ByTag (tagsUSel2 usel2)
                                  vsegids1' vsegids2'

         -- All data from the source arrays goes into the result
        ussegd'   = appendUSSegd ussegd1 pdatas1
                                 ussegd2 pdatas2
                                  
   in   UVSegd vsegids' ussegd'
