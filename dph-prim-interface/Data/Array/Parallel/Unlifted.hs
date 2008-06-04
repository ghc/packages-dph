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

class UA a
instance UA Bool
instance UA Int
instance (UA a, UA b) => UA (a :*: b)
instance UA a => UA [a]

type UArr a = [a]
type SUArr a = [[a]]
type USegd   = ([Int], [Int])

length = P.length
empty = []
replicate = P.replicate
replicateEach n ns xs
  = ASSERT (n == sum ns)
  . concat
  $ zipWith replicate ns xs
repeat n xs = concat (replicate n xs)
(!:) = (P.!!)
bpermute xs ns = map (xs !:) ns
(+:+) = (P.++)

pack xs bs = [x | (x,b) <- P.zip xs bs, b]

combine [] [] [] = []
combine (True  : bs) (x : xs) ys       = x : combine bs xs ys
combine (False : bs) xs       (y : ys) = y : combine bs xs ys

map = P.map
zip = P.zipWith (:*:)
unzip = pairS . P.unzip . P.map unpairS
fsts = map fstS
zip3 = P.zipWith3 (\x y z -> x :*: y :*: z)
unzip3 xs = unzip ys :*: zs
  where
    ys :*: zs = unzip xs
zipWith = P.zipWith
zipWith3 = P.zipWith3

fold = P.foldr -- or equivalently foldl
fold1 = P.foldr1 -- or equivalently foldr1
sum = P.sum
scan f z = P.init . P.scanl f z

indexed xs = zip [0 .. length xs - 1] xs
enumFromTo m n = [m .. n]
enumFromToEach n ps = ASSERT (n == length ns)
                    $ ns
  where
    ns = concat (map (uncurryS enumFromTo) ps)

concat = P.concat
(ns, is) >: xs = go ns xs
  where
    go [] [] = []
    go (n : ns) xs = let (ys, zs) = P.splitAt n xs
                     in ys : go ns zs

(^+:+^) = P.zipWith (+:+)

length_s = P.length
lengths_s = map length
indices_s = scan (+) 0 . lengths_s

fold_s f z = map (fold f z)
fold1_s f = map (fold1 f)

indexed_s = map indexed

toUSegd = unpairS . unzip

