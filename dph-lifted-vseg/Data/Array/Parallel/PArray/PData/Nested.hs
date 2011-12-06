{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UndecidableInstances, ParallelListComp #-}
{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | PR instance for nested arrays.
module Data.Array.Parallel.PArray.PData.Nested 
        ( PData(..)
        , PDatas(..)
        , mkPNested
        , concatPR,     concatlPR
        , flattenPR,    takeSegdPD
        , unconcatPR
        , appendlPR
        , indexlPR
        , slicelPD)
where
import Data.Array.Parallel.Base
import Data.Array.Parallel.Pretty
import Data.Array.Parallel.PArray.PData.Base    as PA
import qualified Data.IntSet                    as IS
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import GHC.Exts

-- Nested arrays --------------------------------------------------------------
data instance PData (PArray a)
        = PNested
        { pnested_uvsegd       :: !U.VSegd
          -- ^ Virtual segmentation descriptor. 
          --   Defines a virtual nested array based on physical data.

        , pnested_psegdata     :: !(PDatas a) 
          -- ^ Chunks of array data, where each chunk has a linear index space. 
        }

-- TODO: should we unpack the vsegd fields here?
data instance PDatas (PArray a)
        = PNesteds (V.Vector (PData (PArray a)))

-- TODO: Do we actually use this? looks weird.
data instance PDatas (PData a)
        = PPDatas (V.Vector (PData a))


-- | Conatruct a nested array.
mkPNested :: U.Array Int        -- ^ Virtual segment ids.
          -> U.Array Int        -- ^ Lengths of physical segments.
          -> U.Array Int        -- ^ Starting indices of physical segments.
          -> U.Array Int        -- ^ Source id (what chunk to get each segment from).
          -> PDatas a           -- ^ Chunks of array data.
          -> PData (PArray a)
mkPNested vsegids pseglens psegstarts psegsrcids psegdata
        = PNested
                (U.mkVSegd vsegids 
                        $ U.mkSSegd psegstarts psegsrcids
                        $ U.lengthsToSegd pseglens)
                psegdata
{-# INLINE_PDATA mkPNested #-}


-- Old projection functions. 
-- TODO: refactor to eliminate the need for these.
pnested_vsegids    :: PData (PArray a) -> U.Array Int
pnested_vsegids    =  U.takeVSegidsOfVSegd . pnested_uvsegd

pnested_pseglens   :: PData (PArray a) -> U.Array Int
pnested_pseglens   =  U.lengthsOfSSegd . U.takeSSegdOfVSegd . pnested_uvsegd

pnested_psegstarts :: PData (PArray a) -> U.Array Int
pnested_psegstarts  = U.startsOfSSegd  . U.takeSSegdOfVSegd . pnested_uvsegd

pnested_psegsrcids :: PData (PArray a) -> U.Array Int
pnested_psegsrcids  = U.sourcesOfSSegd . U.takeSSegdOfVSegd . pnested_uvsegd



-- PR Instances ---------------------------------------------------------------
instance U.Elt (Int, Int, Int)

instance PR a => PR (PArray a) where
  -- TODO: make this check all sub arrays as well
  -- TODO: ensure that all psegdata arrays are referenced from some psegsrc.
  -- TODO: shift segd checks into associated modules.
  {-# NOINLINE validPR #-}
  validPR arr
   = let 
         vsegids        = pnested_vsegids     arr
         pseglens       = pnested_pseglens    arr
         psegstarts     = pnested_psegstarts  arr
         psegsrcs       = pnested_psegsrcids  arr
         psegdata       = pnested_psegdata    arr


        -- The lengths of the pseglens, psegstarts and psegsrcs fields must all be the same
         fieldLensOK
                = validBool "nested array field lengths not identical"
                $ and 
                [ U.length psegstarts == U.length pseglens
                , U.length psegsrcs   == U.length pseglens ]

         -- Every vseg must reference a valid pseg.
         vsegsRefOK
                = validBool "nested array vseg doesn't ref pseg"
                $ U.and
                $ U.map (\vseg -> vseg < U.length pseglens) vsegids
                
         
         -- Every pseg source id must point to a flat data array
         psegsrcsRefOK
                = validBool "nested array psegsrc doesn't ref flat array"
                $ U.and 
                $ U.map (\srcid -> srcid < lengthdPR psegdata) psegsrcs

         -- Every physical segment must be a valid slice of the corresponding flat array.
         -- 
         --   We allow psegs with len 0, start 0 even if the flat array is empty.
         --   This occurs with [ [] ]. 
         -- 
         --   As a generalistion of above, we allow segments with len 0, start <= srclen.
         --   This occurs when there is an empty array as the last segment
         --   For example:
         --        [ [5, 4, 3, 2] [ ] ].
         --        PNested  vsegids:    [0,1]
         --                 pseglens:   [4,0]
         --                 psegstarts: [0,4]  -- last '4' here is <= length of flat array
         --                 psegsrcs:   [0,0]
         --                 PInt        [5, 4, 3, 2]
         --
         psegSlicesOK 
                = validBool "nested array pseg slices are invalid"
                $ U.and 
                $ U.zipWith3 
                        (\len start srcid
                           -> let pdata = psegdata `indexdPR` srcid
                              in  and [ coversPR (len == 0) pdata start
                                      , coversPR True       pdata (start + len) ])
                        pseglens psegstarts psegsrcs

         -- Every pseg must be referenced by some vseg.
         vsegs   = IS.fromList $ U.toList vsegids
         psegsReffedOK
                =  validBool "nested array pseg not reffed by vseg"
                $  (U.length pseglens == 0) 
                || (U.and $ U.map (flip IS.member vsegs) 
                          $ U.enumFromTo 0 (U.length pseglens - 1))

     in  and [ fieldLensOK
             , vsegsRefOK
             , psegsrcsRefOK
             , psegSlicesOK
             , psegsReffedOK ]


  {-# NOINLINE nfPR #-}
  nfPR    = error "nfPR[PArray]: not defined yet"


  {-# NOINLINE similarPR #-}
  similarPR (PArray _ pdata1) (PArray _ pdata2)
        = V.and $ V.zipWith similarPR 
                        (toVectorPR pdata1)
                        (toVectorPR pdata2)


  {-# NOINLINE coversPR #-}
  coversPR weak (PNested vsegd _ ) ix
   | weak       = ix <= (U.length $ U.takeVSegidsOfVSegd vsegd)
   | otherwise  = ix <  (U.length $ U.takeVSegidsOfVSegd vsegd)

  {-# NOINLINE pprpPR #-}
  pprpPR (PArray n# pdata)
        =   (text "PArray " <+> int (I# n#))
        $+$ ( nest 4 
            $ pprpDataPR pdata)

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PNested vsegd pdatas)
        =   text "PNested"
        $+$ ( nest 4
            $ pprp vsegd $$ pprp pdatas)


  -- Constructors -----------------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR = PNested U.emptyVSegd emptydPR


  -- When replicating an array we use the source as the single physical
  -- segment, then point all the virtual segments to it.
  {-# INLINE_PDATA replicatePR #-}
  replicatePR c (PArray n# darr)
   = {-# SCC "replicatePR" #-}
     checkNotEmpty "replicatePR[PArray]" c
   $ let -- Physical segment descriptor contains a single segment.
         ussegd  = U.singletonSSegd (I# n#)
         
         -- All virtual segments point to the same physical segment.
         uvsegd  = U.mkVSegd (U.replicate c 0) ussegd

     in  PNested uvsegd $ singletondPR darr
                

  -- For segmented replicates, we just replicate the vsegids field.
  --
  -- TODO: Does replicate_s really need the whole segd,
  --       or could we get away without creating the indices field?
  --
  -- TODO: If we know the lens does not contain zeros, then we don't need
  --       to cull down the psegs.
  --
  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PNested uvsegd pdata)
   = PNested (U.updateVSegsOfVSegd
                (\vsegids -> U.replicate_s segd vsegids) uvsegd)
             pdata  


  -- Append nested arrays by appending the segment descriptors,
  -- and putting all physical arrays in the result.
  {-# NOINLINE appendPR #-}
  appendPR (PNested uvsegd1 pdatas1) (PNested uvsegd2 pdatas2)
   = PNested    (U.appendVSegd
                        uvsegd1 (lengthdPR pdatas1) 
                        uvsegd2 (lengthdPR pdatas2))
                (pdatas1 `appenddPR` pdatas2)


  -- Performing segmented append requires segments from the physical arrays to
  -- be interspersed, so we need to copy data from the second level of nesting.  
  --
  -- In the implementation we can safely flatten out replication in the vsegs
  -- because the source program result would have this same physical size
  -- anyway. Once this is done we use copying segmented append on the flat 
  -- arrays, and then reconstruct the segment descriptor.
  --
  {-# NOINLINE appendsPR #-}
  appendsPR rsegd segd1 xarr segd2 yarr
   = let (xsegd, xs)    = flattenPR xarr
         (ysegd, ys)    = flattenPR yarr
   
         xsegd' = U.lengthsToSegd 
                $ U.sum_s segd1 (U.lengthsSegd xsegd)
                
         ysegd' = U.lengthsToSegd
                $ U.sum_s segd2 (U.lengthsSegd ysegd)
                
         segd'  = U.lengthsToSegd
                $ U.append_s rsegd segd1 (U.lengthsSegd xsegd)
                                   segd2 (U.lengthsSegd ysegd)

     in  PNested (U.promoteSegdToVSegd segd')
                 (singletondPR 
                  $ appendsPR (U.plusSegd xsegd' ysegd')
                            xsegd' xs
                            ysegd' ys)


  -- Projections ------------------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PNested vsegd _)
        = U.lengthOfVSegd vsegd


  -- To index into a nested array, first determine what segment the index
  -- corresponds to, and extract that as a slice from that physical array.
  {-# INLINE_PDATA indexPR #-}
  indexPR (PNested uvsegd pdatas) ix
   | (pseglen@(I# pseglen#), psegstart, psegsrcid)    <- U.getSegOfVSegd uvsegd ix
   = let !psrc          = pdatas `indexdPR` psegsrcid
         !pdata'        = extractPR psrc psegstart pseglen
     in  PArray pseglen# pdata'


  {-# INLINE_PDATA indexsPR #-}
  indexsPR pdatas@(PNesteds arrs) srcixs
   = let (srcids, ixs)  = U.unzip srcixs
   
   
         -- See Note: psrcoffset
         !psrcoffset    = V.prescanl (+) 0
                        $ V.map (lengthdPR . pnested_psegdata) arrs

         -- length, start and srcid of the segments we're returning.
         --   Note that we need to offset the srcid 
         -- TODO: don't unbox the VSegd for every iteration.
         seginfo :: U.Array (Int, Int, Int)
         seginfo 
          = U.zipWith (\srcid ix -> 
                        let (PNested vsegd _)   = pdatas `indexdPR` srcid
                            (len, start, srcid') = U.getSegOfVSegd vsegd ix
                        in  (len, start, srcid' + (psrcoffset `V.unsafeIndex` srcid)))
                srcids
                ixs

         (pseglens', psegstarts', psegsrcs')    
                        = U.unzip3 seginfo
                
         -- TODO: check that doing lengthsToSegd won't cause overflow
         vsegd'  = U.promoteSSegdToVSegd
                 $ U.mkSSegd psegstarts' psegsrcs'
                 $ U.lengthsToSegd pseglens'
                                 
          -- All flat data arrays in the sources go into the result.
         pdatas' = fromVectordPR
                 $ V.concat $ V.toList 
                 $ V.map (toVectordPR . pnested_psegdata) arrs
   
     in  PNested vsegd' pdatas'

  -- To extract a range of elements from a nested array, perform the extract
  -- on the vsegids field. The `updateVSegsOfUVSegd` function will then filter
  -- out all of the psegs that are no longer reachable from the new vsegids.
  {-# INLINE_PDATA extractPR #-}
  extractPR (PNested uvsegd pdata) start len
   = {-# SCC "extractPR" #-}
     PNested (U.updateVSegsOfVSegd (\vsegids -> U.extract vsegids start len) uvsegd)
             pdata


  --   TODO: cleanup pnested projections
  --         use getSegOfUVSegd like in indexlPR

  -- [Note: psrcoffset]
  -- ~~~~~~~~~~~~~~~~~~
  -- As all the flat data arrays in the sources are present in the result array,
  -- we need to offset the psegsrcs field when combining multiple sources.
  -- 
  -- Exaple
  --  Source Arrays:
  --   arr0  ...
  --         psrcids  :  [0, 0, 0, 1, 1]
  --         psegdata :  [PInt xs1, PInt xs2]
  --
  --   arr1  ... 
  --         psrcids  :  [0, 0, 1, 1, 2, 2, 2]
  --         psegdata :  [PInt ys1, PInt ys2, PInt ys3]
  -- 
  --   Result Array:
  --         psrcids  :  [...]
  --         psegdata :  [PInt xs1, PInt xs2, PInt ys1, PInt ys2, PInt ys3] 
  --
  --  Note that references to flatdata arrays [0, 1, 2] in arr1 need to be offset
  --  by 2 (which is length arr0.psegdata) to refer to the same flat data arrays
  --  in the result.
  -- 
  --  We encode these offsets in the psrcoffset vector:
  --       psrcoffset :  [0, 2]
  --
  {-# NOINLINE extractssPR #-}
  extractssPR (PNesteds arrs) ussegd
   = {-# SCC "extractsPR" #-}
     let segsrcs        = U.sourcesOfSSegd ussegd
         seglens        = U.lengthsOfSSegd ussegd

         vsegids_src    = U.unsafeExtracts_nss ussegd (V.map pnested_vsegids arrs)
         srcids'        = U.replicate_s (U.lengthsToSegd seglens) segsrcs

         -- See Note: psrcoffset
         psrcoffset     = V.prescanl (+) 0 
                        $ V.map (lengthdPR . pnested_psegdata) arrs

         -- Unpack the lens and srcids arrays so we don't need to 
         -- go though all the segment descriptors each time.
         !arrs_pseglens   = V.map pnested_pseglens   arrs
         !arrs_psegstarts = V.map pnested_psegstarts arrs
         !arrs_psegsrcids = V.map pnested_psegsrcids arrs

         -- Function to get one element of the result.
         {-# INLINE get #-}
         get srcid vsegid
          = let !pseglen        = (arrs_pseglens   `V.unsafeIndex` srcid) `U.unsafeIndex` vsegid
                !psegstart      = (arrs_psegstarts `V.unsafeIndex` srcid) `U.unsafeIndex` vsegid
                !psegsrcid      = (arrs_psegsrcids `V.unsafeIndex` srcid) `U.unsafeIndex` vsegid  
                                + psrcoffset `V.unsafeIndex` srcid
            in  (pseglen, psegstart, psegsrcid)
            
         (pseglens', psegstarts', psegsrcs')
                = U.unzip3 $ U.zipWith get srcids' vsegids_src

         -- All flat data arrays in the sources go into the result.
         psegdatas'     = fromVectordPR
                        $ V.concat $ V.toList 
                        $ V.map (toVectordPR . pnested_psegdata) arrs
                   
         -- Build the result segment descriptor.
         vsegd'         = U.promoteSSegdToVSegd
                        $ U.mkSSegd psegstarts' psegsrcs'
                        $ U.lengthsToSegd pseglens'
   
     in  PNested vsegd' psegdatas'

                
  -- Pack and Combine -------------------------------------
  -- Pack the vsegids to determine which of the vsegs are present in the result.
  --  eg  tags:           [0 1 1 1 0 0 0 0 1 0 0 0 0 1 0 1 0 1 1]   tag = 1
  --      vsegids:        [0 0 1 1 2 2 2 2 3 3 4 4 4 5 5 5 5 6 6]
  --  =>  vsegids_packed: [  0 1 1         3         5   5   6 6]
  --       
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PNested uvsegd pdata) tags tag
   = PNested (U.updateVSegsOfVSegd (\vsegids -> U.packByTag vsegids tags tag) uvsegd)
             pdata


  -- Combine nested arrays by combining the segment descriptors, 
  -- and putting all physical arrays in the result.
  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel2 (PNested uvsegd1 pdatas1) (PNested uvsegd2 pdatas2)
   = PNested    (U.combine2VSegd sel2 
                        uvsegd1 (lengthdPR pdatas1)
                        uvsegd2 (lengthdPR pdatas2))
                (pdatas1 `appenddPR` pdatas2)


  -- Conversions ----------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR xx
   | V.length xx == 0 = emptyPR
   | otherwise
   = let segd      = U.lengthsToSegd $ U.fromList $ V.toList $ V.map PA.length xx
     in  mkPNested
                (U.enumFromTo 0 (V.length xx - 1))
                (U.lengthsSegd segd)
                (U.indicesSegd segd)
                (U.replicate (V.length xx) 0)
                (singletondPR (V.foldl1 appendPR $ V.map takeData xx))


  {-# NOINLINE toVectorPR #-}
  toVectorPR arr
   = V.generate (U.length (pnested_vsegids arr))
   $ indexPR arr


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PNesteds $ V.empty
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR pdata
        = PNesteds $ V.singleton pdata

  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PNesteds vec)
        = V.length vec
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PNesteds vec) ix
        = vec `V.unsafeIndex` ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PNesteds xs) (PNesteds ys)
        = PNesteds $ xs V.++ ys

  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = PNesteds vec
        
  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PNesteds vec)
        = vec

------------------------------------------------------------------------------
-- | O(len result). Lifted indexing
indexlPR :: PR a => PData (PArray a) -> PData Int -> PData a
indexlPR (PNested vsegd pdatas) (PInt ixs)
 = indexvsPR pdatas vsegd 
        (U.zip  (U.enumFromTo 0 (U.length ixs - 1))
                ixs)
{-# INLINE_PDATA indexlPR #-}


-------------------------------------------------------------------------------
-- | O(len result). Concatenate a nested array.
--
--   This physically performs a gather operation, whereby array data is copied
--   through the index-space transformation defined by the segment descriptor
--   in the nested array. We must perform this copy because reducing the level
--   of nesting corresponds to discarding the segment descriptor, which means we
--   can no longer represent the layout of the array other than by physically
--   creating it.
--
--   As an optimisation, if the segment descriptor knows that the segments are
--   already in a single contiguous `PData` no sharing, then concat can just
--   return the underlying array directly, in constant time.
-- 
--   WARNING: 
--   Concatenating a replicated array can cause index overflow, because the 
--   source array can define more elements than we can count with a single
--   machine word.
--   For example, if we replicate an array with 1Meg elements 1Meg times then
--   the result defines a total of 1Meg*1Meg = 1Tera elements. This in itself
--   is fine, because the nested array is defined by an index space transform
--   that maps all the inner arrays back to the original data. However, if we 
--   then concatenate the replicated array then we must physically copy the 
--   data as we loose the segment descriptor that defines the mapping. Sad 
--   things will happen when the library tries to construct an physical array
--   1Tera elements long, especially on 32 bit machines.

--   IMPORTANT:
--   In the case where there is sharing between segments, or they are scattered
--   through multiple arrays, only outer-most two levels of nesting are physically
--   merged. The data for lower levels is not touched. This ensures that concat
--   has complexity proportional to the length of the result array, instead
--   of the total number of elements within it.
--
concatPR :: PR a => PData (PArray a) -> PData a
concatPR (PNested vsegd pdatas)

--       TODO: we want to implement this as rewwrite rules instead of 
--              a branch, so we don't get the branch in the core code.
        
        -- If we know that the segments are in a single contiguous array, 
        -- and there is no sharing between them, then we can just return
        -- that array directly.
{-        | U.isManifestVSegd   vsegd
        , U.isContiguousVSegd vsegd
        , lengthdPR pdatas == 1
        = pdatas `indexdPR` 0
-}
        -- Otherwise we have to pull all the segments through the index 
        -- space transform defined by the vsegd, which copies them
        -- into a single contiguous array.
        | otherwise
        = extractvsPR pdatas vsegd
{-# NOINLINE concatPR #-}


-- | Lifted concatenation.
-- 
--   Concatenate all the arrays in a triply nested array.
--
concatlPR :: PR a => PData (PArray (PArray a)) -> PData (PArray a)
concatlPR arr
 = let  (segd1, darr1)  = flattenPR arr
        (segd2, darr2)  = flattenPR darr1

        -- Generate indices for the result array
        --  There is a tedious edge case when the last segment in the nested
        --  array has length 0. For example:
        --
        --    concatl [ [[1, 2, 3] [4, 5, 6]] [] ]
        --  
        --  After the calls to flattenPR we get:
        --   segd1: lengths1 = [ 2 0 ]
        --          indices1 = [ 0 2 ]
        
        --   segd2: lengths2 = [ 3 3 ]
        --          indices2 = [ 0 3 ]
        -- 
        --  The problem is that the last element of 'indices1' points off the end
        --  of 'indices2' so we can't use use 'backpermute' as we'd like to:
        --    ixs' = (U.bpermute (U.indicesSegd segd2) (U.indicesSegd segd1))        
        --  Instead, we have to explicitly check for the out-of-bounds condition.
        --  TODO: We want a faster way of doing this, that doesn't require the 
        --        test for every element.
        -- 
        ixs1            = U.indicesSegd segd1
        ixs2            = U.indicesSegd segd2
        len2            = U.length ixs2

        ixs'            = U.map (\ix -> if ix >= len2
                                                then 0
                                                else ixs2 `U.unsafeIndex` ix)
                        $ ixs1

        segd'           = U.mkSegd (U.sum_s segd1 (U.lengthsSegd segd2))
                                   ixs'
                                   (U.elementsSegd segd2)

   in   PNested (U.promoteSegdToVSegd segd') 
                (singletondPR darr2)

{-# INLINE_PDATA concatlPR #-}


-- | Build a nested array given a single flat data vector, 
--   and a template nested array that defines the segmentation.

--   Although the template nested array may be using vsegids to describe
--   internal sharing, the provided data array has manifest elements
--   for every segment. Because of this we need flatten out the virtual
--   segmentation of the template array.
--
--   WARNING:
--   This can cause index space overflow, see the note in `concatPR`.
--
unconcatPR :: PR b => PData (PArray a) -> PData b -> PData (PArray b)
unconcatPR (PNested vsegd _) pdata
 = {-# SCC "unconcatPD" #-}
   let  
        -- Demote the vsegd to a manifest vsegd so it contains all the segment
        -- lengths individually without going through the vsegids.
        !segd           = U.unsafeDemoteToSegdOfVSegd vsegd

        -- Rebuild the vsegd based on the manifest vsegd. 
        -- The vsegids will be just [0..len-1], but this field is constructed
        -- lazilly and consumers aren't required to demand it.
        !vsegd'         = U.promoteSegdToVSegd segd

   in   PNested vsegd' (singletondPR pdata)
{-# INLINE_PDATA unconcatPR #-}


-- | Flatten a nested array, yielding a plain segment descriptor and 
--   concatenated data.
--
--   WARNING:
--   This can cause index space overflow, see the note in `concatPR`.
--
flattenPR :: PR a => PData (PArray a) -> (U.Segd, PData a)
flattenPR arr@(PNested uvsegd _)
 =      ( U.unsafeDemoteToSegdOfVSegd uvsegd
        , concatPR arr)
{-# INLINE_PDATA flattenPR #-}


-- | Lifted append.
--   Both arrays must contain the same number of elements.
appendlPR :: PR a => PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPR  arr1 arr2
 = let  (segd1, darr1)  = flattenPR arr1
        (segd2, darr2)  = flattenPR arr2
        segd'           = U.plusSegd segd1 segd2
   in   PNested (U.promoteSegdToVSegd segd' )
                (singletondPR
                 $ appendsPR segd' segd1 darr1 segd2 darr2)
{-# INLINE_PDATA appendlPR #-}



-- PD Functions ---------------------------------------------------------------
-- These functions work on nested PData arrays, but don't need a PR or PA
-- dictionary. They are segment descriptor operations that only care about the
-- outermost later of segmentation, and thus are oblivous to the element type.
--

-- | Take the segment descriptor from a nested array and demote it to a
--   plain Segd.
-- 
--   WARNING:
--   This can cause index space overflow, see the note in `concatPR`.
--
takeSegdPD :: PData (PArray a) -> U.Segd
takeSegdPD (PNested vsegd _) 
        = U.unsafeDemoteToSegdOfVSegd vsegd
{-# INLINE_PDATA takeSegdPD #-}


-- | Extract some slices from some arrays.
--
--   All three parameters must have the same length, and we take
--   one slice from each of the source arrays. 

--   TODO: cleanup pnested projections
slicelPD
        :: PData Int            -- ^ Starting indices of slices.
        -> PData Int            -- ^ Lengths of slices.
        -> PData (PArray a)     -- ^ Arrays to slice.
        -> PData (PArray a)

slicelPD (PInt sliceStarts) (PInt sliceLens) arr
 = {-# SCC "slicelDD" #-}
   let  segs            = U.length vsegids
        vsegids        = pnested_vsegids     arr
        psegstarts     = pnested_psegstarts  arr
        psegsrcs       = pnested_psegsrcids  arr
        psegdata       = pnested_psegdata    arr
   in   
        mkPNested
                (U.enumFromTo 0 (segs - 1))
                sliceLens
                (U.zipWith (+) (U.bpermute psegstarts vsegids) sliceStarts)
                (U.bpermute psegsrcs vsegids)
                psegdata

{-# NOINLINE slicelPD #-}
--  NOINLINE because it won't fuse with anything.
--  The operation is also entierly on the segment descriptor, so we don't 
--  need to inline it to specialise it for the element type.


-- Testing --------------------------------------------------------------------
-- TODO: slurp debug flag from base 
validBool :: String -> Bool -> Bool
validBool str b
        = if b  then True 
                else error $ "validBool check failed -- " ++ str


-- Pretty ---------------------------------------------------------------------
deriving instance Show (PDatas a) => Show (PDatas (PArray a))
deriving instance Show (PDatas a) => Show (PData  (PArray a))


