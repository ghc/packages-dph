{-# LANGUAGE TypeOperators, CPP #-}

-- | This module provides the API for the DPH backend. 
--
--   These are the DPH array primitives that the vectoriser introduces when
--   transforming code. The actual code in this module is fake, in the sense
--   that is provides a partial reference implementation using lists to
--   represent arrays, but this code isn't acually used at runtime.
--
--   The actual code used by compiled programs depends on whether @-fdph-par@ or
--   @-fdph-seq@ is passed  when compiling it. Depending on the flag, the
--   implementation in either the @dph-prim-par@ or @dph-prim-seq packages@ is
--   swapped in. These packages export the same API, but use a more efficient, 
--   and perhaps parallel implementation.
--
--   All three packages are forced to use the same API by the 'DPH_Header.h'
--   and 'DPH_Interface.h' include files in @dph-prim-interface/interface@.
--
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


-- Basics ---------------------------------------------------------------------
class Elt a
instance Elt a => Elt [a]
type Array a    = [a]


-- Constructors ---------------------------------------------------------------
empty                   = []
(+:+)                   = (P.++)
replicate               = P.replicate
repeat n _ xs           = P.concat (replicate n xs)
indexed xs              = zip [0 .. length xs - 1] xs
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
extract xs i n  = P.take n (P.drop i xs)
drop            = P.drop
filter          = P.filter
permute         = notImplemented "permute"
bpermute xs ns  = map (xs !:) ns
mbpermute       = notImplemented "mbpermute"
bpermuteDft     = notImplemented "bpermuteDft"


-- Update ---------------------------------------------------------------------
update           = notImplemented "update"


-- Packing and Combining ------------------------------------------------------
pack xs bs      = [x | (x,b) <- P.zip xs bs, b]

combine [] [] [] = []
combine (True  : bs) (x : xs) ys       = x : combine bs xs ys
combine (False : bs) xs       (y : ys) = y : combine bs xs ys

combine2 tags _ xs ys = go tags xs ys
  where
    go [] [] [] = []
    go (0 : bs) (x : xs) ys = x : go bs xs ys
    go (1 : bs) xs (y : ys) = y : go bs xs ys

interleave xs ys = P.concat [[x,y] | (x,y) <- P.zip xs ys]


-- Map and zipWith ------------------------------------------------------------
map             = P.map
zipWith         = P.zipWith


-- Zipping and Unzipping ------------------------------------------------------
zip             = P.zip
unzip           = P.unzip
fsts            = map P.fst
snds            = map P.snd

zip3            = P.zip3
unzip3          = P.unzip3


-- Folds ----------------------------------------------------------------------
fold            = P.foldr
fold1           = P.foldr1
and             = P.and
sum             = P.sum
scan f z        = P.init . P.scanl f z


-- Segmented Constructors -----------------------------------------------------
append_s _ xd xs yd ys 
        = P.concat (P.zipWith (P.++) (nest xd xs) (nest yd ys))

replicate_s segd xs
        = P.concat
        $ zipWith replicate (lengthsSegd segd) xs

replicate_rs n xs
        = P.concat
        $ P.map (P.replicate n) xs


-- Segmented Projections ------------------------------------------------------
indices_s segd
        = P.concat [[0 .. n-1] | n <- segd_lengths segd] 


-- Segmented Folds ------------------------------------------------------------
fold_s  f z segd xs
        = P.map (P.foldr f z) (nest segd xs)

fold1_s f   segd xs
        = P.map (P.foldr1 f)  (nest segd xs)

fold_r  f z segSize xs 
        = notImplemented "fold_r"
        
sum_r segSize xs 
        = notImplemented "sum_r"


-- Scattered Segmented Folds --------------------------------------------------
fold_ss  = notImplemented "fold_ss"
fold1_ss = notImplemented "fold1_ss"


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


-- Slice Segment Descriptors --------------------------------------------------
data SSegd
        = SSegd
        { ssegd_starts  :: [Int]
        , ssegd_sources :: [Int]
        , ssegd_segd    :: Segd }

mkSSegd                 = SSegd
emptySSegd              = SSegd [] [] emptySegd
singletonSSegd          = notImplemented "singletonSSegd"
validSSegd              = notImplemented "validSSegd"
promoteSegdToSSegd      = notImplemented "promoteSegdToSSegd"
lengthSSegd             = lengthSegd  . ssegd_segd
lengthsSSegd            = lengthsSegd . ssegd_segd
indicesSSegd            = indicesSegd . ssegd_segd
startsSSegd             = ssegd_starts
sourcesSSegd            = ssegd_sources
getSegOfSSegd           = notImplemented "getSegOfSSegd"
appendSSegd             = notImplemented "appendSSegd"


-- Virtual Segment Descriptors ------------------------------------------------
data VSegd
        = VSegd
        { vsegd_vsegids :: [Int]
        , vsegd_ssegd   :: SSegd }

mkVSegd                 = VSegd
validVSegd              = notImplemented "validSSegd"       
promoteSegdToVSegd      = notImplemented "promoteSegdToVSegd"
promoteSSegdToVSegd     = notImplemented "promoteSSegdToVSegd"
emptyVSegd              = VSegd [] emptySSegd
singletonVSegd          = notImplemented "singletonVSegd"
lengthOfVSegd           = notImplemented "lengthOfVSegd"
takeVSegidsOfVSegd      = vsegd_vsegids
takeSSegdOfVSegd        = vsegd_ssegd
takeLengthsOfVSegd      = notImplemented "takeLengthsOfVSegd"
getSegOfVSegd           = notImplemented "getSegOfVSegd"
demoteToSSegdOfVSegd    = notImplemented "demoteToSSegdOfVSegd"
demoteToSegdOfVSegd     = notImplemented "demoteToSegdOfVSegd"
updateVSegsOfVSegd      = notImplemented "updateVSegsOfVSegd"
appendVSegd             = notImplemented "appendVSegd"
combine2VSegd           = notImplemented "combine2VSegd"


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


-- Selector Representations ---------------------------------------------------
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
