{-# LANGUAGE TypeOperators, CPP #-}

#include "DPH_Header.h"

import Data.Array.Parallel.Base

import qualified Prelude as P
import Prelude ( Eq(..), Num(..), Bool(..), ($), (.) )

#include "DPH_Interface.h"

#define ASSERT assert __FILE__ __LINE__

assert :: P.String -> Int -> Bool -> a -> a
assert file line False _
  = P.error $ file P.++ " (line " P.++ P.show line P.++ "): assertion failure"
assert _ _ _ x = x

class Elt a
instance Elt a => Elt [a]

type Array a = [a]
data Segd = Segd { segd_lengths  :: [Int]
                 , segd_indices  :: [Int]
                 , segd_elements :: Int
                 }

length = P.length
empty = []
replicate = P.replicate
repeat n _ xs = P.concat (replicate n xs)
(!:) = (P.!!)
extract xs i n = P.take n (P.drop i xs)
drop = P.drop
permute = P.error "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted.permute"
bpermute xs ns = map (xs !:) ns
update = P.error "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted.update"
(+:+) = (P.++)

mbpermute = P.error "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted.mbpermute"
bpermuteDft = P.error "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted.bpermuteDft"


pack xs bs = [x | (x,b) <- P.zip xs bs, b]

combine [] [] [] = []
combine (True  : bs) (x : xs) ys       = x : combine bs xs ys
combine (False : bs) xs       (y : ys) = y : combine bs xs ys

map = P.map
filter = P.filter
zip = P.zipWith (:*:)
unzip = pairS . P.unzip . P.map unpairS
fsts = map fstS
snds = map sndS
zip3 = P.zipWith3 (\x y z -> x :*: y :*: z)
unzip3 xs = unzip ys :*: zs
  where
    ys :*: zs = unzip xs
zipWith = P.zipWith
zipWith3 = P.zipWith3

fold = P.foldr -- or equivalently foldl
fold1 = P.foldr1 -- or equivalently foldr1
and = P.and
sum = P.sum
scan f z = P.init . P.scanl f z

indexed xs = zip [0 .. length xs - 1] xs
enumFromTo m n = [m .. n]
enumFromThenTo m n s = [m, n..s]
enumFromToEach n ps = ASSERT (n == length ns)
                    $ ns
  where
    ns = P.concat (map (uncurryS enumFromTo) ps)

enumFromStepLen i k 0 = []
enumFromStepLen i k n = i : enumFromStepLen (i+k) k (n-1)

enumFromStepLenEach size  fsl =  ASSERT (size == (sum (P.map (\(x :*: y :*: z) -> z) fsl))) $
  P.concat $ P.map (\(x :*:y :*:z) -> P.enumFromThenTo x (x+y) (x+ y*z)) fsl
  

randoms n = P.take n . System.Random.randoms
randomRs n r = P.take n . System.Random.randomRs r

nest :: Segd -> [a] -> [[a]]
nest (Segd ns is _) xs = go ns xs
  where
    go [] [] = []
    go (n : ns) xs = let (ys, zs) = P.splitAt n xs
                     in ys : go ns zs

replicate_s segd xs
  = P.concat
  $ zipWith replicate (lengthsSegd segd) xs
replicate_rs n xs
  = P.concat
  $ P.map (P.replicate n) xs
append_s xd xs yd ys = P.concat (P.zipWith (P.++) (nest xd xs) (nest yd ys))

fold_s  f z segd xs = P.map (P.foldr f z) (nest segd xs)
fold1_s f   segd xs = P.map (P.foldr1 f)  (nest segd xs)
sum_r _ segSize xs = P.error "FIXME GABI PLEASE PLEASE PLEASE" 

lengthSegd = length . lengthsSegd
lengthsSegd = segd_lengths
indicesSegd = segd_indices
elementsSegd = segd_elements
lengthsToSegd lens = Segd lens (scan (+) 0 lens) (sum lens)
mkSegd = Segd

class Elt a => IOElt a
hPut = P.error "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted.hPut"
hGet = P.error "Not implemented: dph-prim-interface:Data.Array.Parallel.Unlifted.hGet"

toList x = x
fromList x = x

toList_s x = x
fromList_s x = x

