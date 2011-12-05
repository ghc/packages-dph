{-# LANGUAGE TypeOperators, CPP #-}

-- | WARNING: This is a fake module and most of the functions here will just `error` if called. 
--
--   This "dph-prim-interface" module provides an API and a partial reference
--   implentation for the unlifted array primitives used by DPH. This code is 
--   not used during runtime.
--
--  Client programs should use the @dph-prim-seq@ or @dph-prim-par@ packages
--  instead, provide the same API and contain real code.
--

--  NOTE: The API is enforced by the DPH_Header.h and DPH_Interface.h headers.
--  The dph-prim-interface, dph-prim-seq, and dph-prim-par modules all import
--  the same headers so we can be sure we're presenting the same API.
#include "DPH_Header.h"

import qualified Prelude as P
import Prelude ( Eq(..), Num(..), Bool(..), ($), (.) )

#include "DPH_Interface.h"

-- NOTE -----------------------------------------------------------------------
-- See DPH_Interface.h for documentation. 
--   As these functions are defined multiple times in different packages, 
--   we keep all the docs there.
--
-- The definitions should appear in the same order as they are defined in DPH_Interface.h
--
#define ASSERT assert __FILE__ __LINE__

assert :: P.String -> Int -> Bool -> a -> a
assert file line False _
  = P.error $ file P.++ " (line " P.++ P.show line P.++ "): assertion failure"
assert _ _ _ x = x

notImplemented :: P.String -> a
notImplemented fnName
        = P.error $ "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted." P.++ fnName


-- Types ----------------------------------------------------------------------
class Elt a
instance Elt a => Elt [a]
type Array a    = [a]


-- Constructors ---------------------------------------------------------------
empty   = []

(+:+)   = (P.++)

append_s _ xd xs yd ys 
        = P.concat (P.zipWith (P.++) (nest xd xs) (nest yd ys))

replicate
        = P.replicate

replicate_s segd xs
        = P.concat
        $ zipWith replicate (lengthsSegd segd) xs

replicate_rs n xs
        = P.concat
        $ P.map (P.replicate n) xs

repeat n _ xs
        = P.concat (replicate n xs)

indexed xs
        = zip [0 .. length xs - 1] xs

indices_s segd
        = P.concat [[0 .. n-1] | n <- segd_lengths segd] 

enumFromTo m n          = [m .. n]
enumFromThenTo m n s    = [m, n..s]
enumFromStepLen i k 0   = []
enumFromStepLen i k n   = i : enumFromStepLen (i+k) k (n-1)

enumFromStepLenEach size starts steps lens
  = ASSERT (size == sum lens)
    P.concat
  $ P.zipWith3 (\x y z -> P.enumFromThenTo x (x+y) (x+y*z)) starts steps lens


-- Projections ----------------------------------------------------------------
length          = P.length
(!:)            = (P.!!)
unsafeIndex     = (P.!!)

extract xs i n   = P.take n (P.drop i xs)
unsafeExtract_ss = notImplemented "unsafeExtract_ss"
unsafeExtract_vs = notImplemented "unsafeExtract_vs"

drop            = P.drop


-- Update ---------------------------------------------------------------------
update           = notImplemented "update"


-- Permutation ----------------------------------------------------------------
permute         = notImplemented "permute"
bpermute xs ns  = map (xs !:) ns
mbpermute       = notImplemented "mbpermute"
bpermuteDft     = notImplemented "bpermuteDft"


-- Zipping and Unzipping ------------------------------------------------------
zip             = P.zip
zip3            = P.zip3
unzip           = P.unzip
unzip3          = P.unzip3
fsts            = map P.fst
snds            = map P.snd


-- Map and zipWith ------------------------------------------------------------
map             = P.map
zipWith         = P.zipWith


-- Scans and Folds -----------------------------------------------------------
scan f z
        = P.init . P.scanl f z

fold    = P.foldr

fold_s  f z segd xs
        = P.map (P.foldr f z) (nest segd xs)

fold_ss = notImplemented "fold_ss"

fold_r  f z segSize xs 
        = notImplemented "fold_r"

fold1   = P.foldr1

fold1_s f   segd xs
        = P.map (P.foldr1 f)  (nest segd xs)

fold1_ss = notImplemented "fold1_ss"

sum     = P.sum

sum_r segSize xs 
        = notImplemented "sum_r"

and     = P.and


-- Packing and Combining ------------------------------------------------------
pack xs bs
        = [x | (x,b) <- P.zip xs bs, b]

filter  = P.filter


-- Combine and Interleave -----------------------------------------------------
combine [] [] [] = []
combine (True  : bs) (x : xs) ys       = x : combine bs xs ys
combine (False : bs) xs       (y : ys) = y : combine bs xs ys

combine2 tags _ xs ys = go tags xs ys
  where
    go [] [] [] = []
    go (0 : bs) (x : xs) ys = x : go bs xs ys
    go (1 : bs) xs (y : ys) = y : go bs xs ys

interleave xs ys = P.concat [[x,y] | (x,y) <- P.zip xs ys]


-- Selectors ------------------------------------------------------------------
data Sel2 
        = Sel2 
        { sel2_tags      :: [Tag]
        , sel2_indices   :: [Int]
        , sel2_elements0 :: Int
        , sel2_elements1 :: Int }

mkSel2 tags idxs n0 n1 _ 
        = Sel2 tags idxs n0 n1

tagsSel2        = sel2_tags
indicesSel2     = sel2_indices
elementsSel2_0  = sel2_elements0
elementsSel2_1  = sel2_elements1
repSel2 _       = ()


type SelRep2    = ()
mkSelRep2 _     = ()

indicesSelRep2 tags _ 
  = P.zipWith pick tags
  $ P.init
  $ P.scanl add (0,0) tags
  where
    pick 0 (i,j) = i
    pick 1 (i,j) = j

    add (i,j) 0 = (i+1,j)
    add (i,j) 1 = (i,j+1)

elementsSelRep2_0 tags _ = P.length [() | 0 <- tags]
elementsSelRep2_1 tags _ = P.length [() | 1 <- tags]



-- Segment Descriptors --------------------------------------------------------
data Segd 
        = Segd 
        { segd_lengths  :: [Int]
        , segd_indices  :: [Int]
        , segd_elements :: Int }

mkSegd                  = Segd
emptySegd               = Segd [] [] 0
singletonSegd           = notImplemented "singletonSegd"
validSegd               = notImplemented "validSegd"
lengthSegd              = length . lengthsSegd
lengthsSegd             = segd_lengths
indicesSegd             = segd_indices
elementsSegd            = segd_elements


-- Scattered Segment Descriptors ----------------------------------------------
data SSegd
        = SSegd
        { ssegd_starts  :: [Int]
        , ssegd_sources :: [Int]
        , ssegd_segd    :: Segd }

mkSSegd                 = SSegd
validSSegd              = notImplemented "validSSegd"
emptySSegd              = SSegd [] [] emptySegd
singletonSSegd          = notImplemented "singletonSSegd"
promoteSegdToSSegd      = notImplemented "promoteSegdToSSegd"
isContiguousSSegd       = notImplemented "isContiguousSSegd"
lengthOfSSegd           = lengthSegd  . ssegd_segd
lengthsOfSSegd          = lengthsSegd . ssegd_segd
indicesOfSSegd          = indicesSegd . ssegd_segd
startsOfSSegd           = ssegd_starts
sourcesOfSSegd          = ssegd_sources
getSegOfSSegd           = notImplemented "getSegOfSSegd"
appendSSegd             = notImplemented "appendSSegd"


-- Virtual Segment Descriptors ------------------------------------------------
data VSegd
        = VSegd
        { vsegd_vsegids :: [Int]
        , vsegd_ssegd   :: SSegd }

mkVSegd                         = VSegd
validVSegd                      = notImplemented "validSSegd"       
promoteSegdToVSegd              = notImplemented "promoteSegdToVSegd"
promoteSSegdToVSegd             = notImplemented "promoteSSegdToVSegd"
isContiguousVSegd               = notImplemented "isContiguousVSegd"
isManifestVSegd                 = notImplemented "isManifestVSegd"
emptyVSegd                      = VSegd [] emptySSegd
singletonVSegd                  = notImplemented "singletonVSegd"
lengthOfVSegd                   = notImplemented "lengthOfVSegd"
takeVSegidsOfVSegd              = vsegd_vsegids
takeVSegidsRedundantOfVSegd     = vsegd_vsegids
takeSSegdOfVSegd                = vsegd_ssegd
takeSSegdRedundantOfVSegd       = vsegd_ssegd
takeLengthsOfVSegd              = notImplemented "takeLengthsOfVSegd"
getSegOfVSegd                   = notImplemented "getSegOfVSegd"
demoteToSSegdOfVSegd            = notImplemented "demoteToSSegdOfVSegd"
unsafeDemoteToSegdOfVSegd       = notImplemented "unsafeDemoteToSegdOfVSegd"
updateVSegsOfVSegd              = notImplemented "updateVSegsOfVSegd"
updateVSegsReachableOfVSegd     = notImplemented "updateVSegsReachableOfVSegd"
appendVSegd                     = notImplemented "appendVSegd"
combine2VSegd                   = notImplemented "combine2VSegd"


-- 2D Arrays ------------------------------------------------------------------
class Elts a
type Arrays a    = [[a]]
emptys           = notImplemented "emptys"
lengths          = notImplemented "lengths"
singletons       = notImplemented "singletons"
unsafeIndexs     = notImplemented "unsafeIndexs"
unsafeIndex2s    = notImplemented "unsafeIndex2s"
appends          = notImplemented "appends"
fromVectors      = notImplemented "fromVectors"
toVectors        = notImplemented "toVectors"


-- Random Arrays --------------------------------------------------------------
randoms n       = P.take n . System.Random.randoms
randomRs n r    = P.take n . System.Random.randomRs r


-- Array IO -------------------------------------------------------------------
class Elt a => IOElt a
hPut            = notImplemented "hPut"
hGet            = notImplemented "hGet"

toList x        = x
fromList x      = x


-- Other Stuff ----------------------------------------------------------------
nest :: Segd -> [a] -> [[a]]
nest (Segd ns is _) xs = go ns xs
  where
    go [] [] = []
    go (n : ns) xs = let (ys, zs) = P.splitAt n xs
                     in ys : go ns zs

toList_s x      = x
fromList_s x    = x

