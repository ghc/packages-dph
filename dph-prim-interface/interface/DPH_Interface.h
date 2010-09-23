import Data.Array.Parallel.Base ( Tag, tagToInt, fromBool )
import qualified GHC.Base
import Prelude ((.), ($), Num(..), Eq(..), seq)
import qualified Prelude

instance Elt Int
instance Elt Word8
instance Elt Bool
instance Elt Float
instance Elt Double
instance (Elt a, Elt b) => Elt (a, b)

infixl 9 !:
infixr 5 +:+
--infixr 5 ^+:+^
--infixr 9 >:

length :: Elt a => Array a -> Int
{-# INLINE_BACKEND length #-}

generate :: Elt a => Int -> (Int -> a) -> Array a
{-# INLINE_BACKEND generate #-}
generate n f = map f (enumFromTo 0 (n-1))

empty :: Elt a => Array a
{-# INLINE_BACKEND empty #-}

replicate :: Elt a => Int -> a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate #-}

repeat :: Elt a => Int -> Int -> Array a -> Array a
{-# INLINE_BACKEND repeat #-}

(!:) :: Elt a => Array a -> Int -> a
{-# INLINE_BACKEND (!:) #-}

extract :: Elt a => Array a -> Int -> Int -> Array a
{-# INLINE_BACKEND extract #-}

drop :: Elt a => Int -> Array a -> Array a
{-# INLINE_BACKEND drop #-}

permute :: Elt a => Array a -> Array Int -> Array a
{-# INLINE_BACKEND permute #-}

bpermute :: Elt a => Array a -> Array Int -> Array a
{-# INLINE_BACKEND bpermute #-}

mbpermute :: (Elt a, Elt b) => (a->b) ->Array a -> Array Int -> Array b
{-# INLINE_BACKEND mbpermute #-}

bpermuteDft:: Elt e => Int -> (Int -> e) -> Array (Int, e) -> Array e
{-# INLINE_BACKEND bpermuteDft #-}

update :: Elt a => Array a -> Array (Int, a) -> Array a
{-# INLINE_BACKEND update #-}

(+:+) :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND (+:+) #-}

interleave :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND interleave #-}

pack :: Elt a => Array a -> Array Bool -> Array a
{-# INLINE_BACKEND pack #-}

combine :: Elt a => Array Bool -> Array a -> Array a -> Array a
{-# INLINE_BACKEND combine #-}

combine2 :: Elt a => Array Tag -> SelRep2 -> Array a -> Array a -> Array a
{-# INLINE_BACKEND combine2 #-}

map :: (Elt a, Elt b) => (a -> b) -> Array a -> Array b
{-# INLINE_BACKEND map #-}

filter :: Elt a => (a -> Bool) -> Array a -> Array a
{-# INLINE_BACKEND filter #-}

zip :: (Elt a, Elt b) => Array a -> Array b -> Array (a, b)
{-# INLINE CONLIKE PHASE_BACKEND zip #-}

unzip :: (Elt a, Elt b) => Array (a, b) -> (Array a, Array b)
{-# INLINE_BACKEND unzip #-}

fsts  :: (Elt a, Elt b) => Array (a, b) -> Array a
{-# INLINE_BACKEND fsts #-}

snds :: (Elt a, Elt b) => Array (a, b) -> Array b
{-# INLINE_BACKEND snds #-}

zipWith :: (Elt a, Elt b, Elt c)
        => (a -> b -> c) -> Array a -> Array b -> Array c
{-# INLINE_BACKEND zipWith #-}

{-# RULES

"zipWith/replicate" forall f m n x y.
  zipWith f (replicate m x) (replicate n y) = replicate m (f x y)

"zipWith/plusInt0_1" forall n xs.
  zipWith GHC.Base.plusInt (replicate n (GHC.Base.I# 0#)) xs = xs

"zipWith/plusInt0_2" forall n xs.
  zipWith GHC.Base.plusInt xs (replicate n (GHC.Base.I# 0#)) = xs

"zipWith(plusInt)/enumFromStepLen" forall i1 k1 n1 i2 k2 n2.
  zipWith GHC.Base.plusInt (enumFromStepLen i1 k1 n1)
                           (enumFromStepLen i2 k2 n2)
    = enumFromStepLen (i1+i2) (k1+k2) n1
  #-}

zipWith3 :: (Elt a, Elt b, Elt c, Elt d)
          => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
{-# INLINE zipWith3 #-}
zipWith3 f xs ys zs = zipWith (\p z -> case p of
                                         (x,y) -> f x y z) (zip xs ys) zs

fold :: Elt a => (a -> a -> a) -> a -> Array a -> a
{-# INLINE_BACKEND fold #-}

fold1 :: Elt a => (a -> a -> a) -> Array a -> a
{-# INLINE_BACKEND fold1 #-}

and :: Array Bool -> Bool
{-# INLINE_BACKEND and #-}

sum :: (Num a, Elt a) => Array a -> a
{-# INLINE_BACKEND sum #-}

{-# RULES

"seq/sum" forall xs e.
  seq (sum xs) e = seq xs e

  #-}

scan :: Elt a => (a -> a -> a) -> a -> Array a -> Array a
{-# INLINE_BACKEND scan #-}

{-# RULES

"seq/scan<Int> (+)" forall i xs e.
  seq (scan GHC.Base.plusInt i xs) e = i `seq` xs `seq` e

  #-}


indexed :: Elt a => Array a -> Array (Int, a)
{-# INLINE_BACKEND indexed #-}

enumFromTo :: Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromTo #-}

enumFromThenTo :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromThenTo #-}

enumFromStepLen :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromStepLen #-}

enumFromStepLenEach :: Int -> Array Int -> Array Int -> Array Int -> Array Int
{-# INLINE_BACKEND enumFromStepLenEach #-}

replicate_s :: Elt a => Segd -> Array a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate_s #-}

replicate_rs :: Elt a => Int -> Array a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate_rs #-}

append_s :: Elt a => Segd         -- ^ segment descriptor of result array
                  -> Segd         -- ^ segment descriptor of first array
                  -> Array a      -- ^ data of first array
                  -> Segd         -- ^ segment descriptor of second array
                  -> Array a      -- ^ data of first array
                  -> Array a
{-# INLINE_BACKEND append_s #-}

{-# RULES

"append_s->interleave" forall n k idxs1 idxs2 idxs3 m1 m2 m3 xs ys.
  append_s (mkSegd (replicate n k) idxs1 m1)
           (mkSegd (replicate n (GHC.Base.I# 1#)) idxs2 m2) xs
           (mkSegd (replicate n (GHC.Base.I# 1#)) idxs3 m3) ys
    = interleave xs ys

  #-}

fold_s :: Elt a => (a -> a -> a) -> a -> Segd -> Array a -> Array a
{-# INLINE_BACKEND fold_s #-}

fold1_s :: Elt a => (a -> a -> a) -> Segd -> Array a -> Array a
{-# INLINE_BACKEND fold1_s #-}

fold_r :: Elt a => (a -> a -> a) -> a -> Int -> Array a -> Array a
{-# INLINE_BACKEND fold_r #-}

sum_s :: (Num a, Elt a) => Segd -> Array a -> Array a
{-# INLINE sum_s #-}
sum_s = fold_s (Prelude.+) 0

sum_r :: (Num a, Elt a) => Int ->Array a -> Array a
{-# INLINE_BACKEND sum_r #-}

indices_s :: Segd -> Array Int
{-# INLINE_BACKEND indices_s #-}
{-
indices_s m segd n = enumFromToEach n
                   . zip (replicate m 0)
                   . map (Prelude.subtract 1)
                   $ lengthsSegd segd
-}

lengthSegd :: Segd -> Int
{-# INLINE_BACKEND lengthSegd #-}

lengthsSegd :: Segd -> Array Int
{-# INLINE_BACKEND lengthsSegd #-}

indicesSegd :: Segd -> Array Int
{-# INLINE_BACKEND indicesSegd #-}

elementsSegd :: Segd -> Int
{-# INLINE_BACKEND elementsSegd #-}

lengthsToSegd :: Array Int -> Segd
{-# INLINE lengthsToSegd #-}
lengthsToSegd ns = mkSegd ns (scan (+) 0 ns) (sum ns)

mkSegd :: Array Int -> Array Int -> Int -> Segd
{-# INLINE CONLIKE PHASE_BACKEND mkSegd #-}

plusSegd :: Segd -> Segd -> Segd
{-# INLINE plusSegd #-}
plusSegd segd1 segd2
  = mkSegd (zipWith (+) (lengthsSegd segd1) (lengthsSegd segd2))
           (zipWith (+) (indicesSegd segd1) (indicesSegd segd2))
           (elementsSegd segd1 `dph_plus` elementsSegd segd2)

{-# RULES

"lengthsSegd/mkSegd" forall lens idxs n.
  lengthsSegd (mkSegd lens idxs n) = lens

"indicesSegd/mkSegd" forall lens idxs n.
  indicesSegd (mkSegd lens idxs n) = idxs

"elementsSegd/mkSegd" forall lens idxs n.
  elementsSegd (mkSegd lens idxs n) = n

"seq/elementsSegd" forall segd e.
  seq (elementsSegd segd) e = seq segd e

"seq/mkSegd" forall lens idxs n e.
  seq (mkSegd lens idxs n) e = lens `seq` idxs `seq` n `seq` e

 #-}

mkSel2 :: Array Tag -> Array Int -> Int -> Int -> SelRep2 -> Sel2
{-# INLINE CONLIKE PHASE_BACKEND mkSel2 #-}

tagsSel2 :: Sel2 -> Array Tag
{-# INLINE_BACKEND tagsSel2 #-}

indicesSel2 :: Sel2 -> Array Int
{-# INLINE_BACKEND indicesSel2 #-}

elementsSel2_0 :: Sel2 -> Int
{-# INLINE_BACKEND elementsSel2_0 #-}

elementsSel2_1 :: Sel2 -> Int
{-# INLINE_BACKEND elementsSel2_1 #-}

repSel2 :: Sel2 -> SelRep2
{-# INLINE_BACKEND repSel2 #-}

mkSelRep2 :: Array Tag -> SelRep2
{-# INLINE CONLIKE PHASE_BACKEND mkSelRep2 #-}

indicesSelRep2 :: Array Tag -> SelRep2 -> Array Int
{-# INLINE_BACKEND indicesSelRep2 #-}

elementsSelRep2_0 :: Array Tag -> SelRep2 -> Int
{-# INLINE_BACKEND elementsSelRep2_0 #-}

elementsSelRep2_1 :: Array Tag -> SelRep2 -> Int
{-# INLINE_BACKEND elementsSelRep2_1 #-}

tagsToSel2 :: Array Tag -> Sel2
{-# INLINE tagsToSel2 #-}
tagsToSel2 tags = let rep = mkSelRep2 tags
                  in
                  mkSel2 tags (indicesSelRep2    tags rep)
                              (elementsSelRep2_0 tags rep)
                              (elementsSelRep2_1 tags rep)
                              rep

{-# RULES

"tagsSel2/mkSel2"
  forall ts is n0 n1 r. tagsSel2 (mkSel2 ts is n0 n1 r) = ts
"indicesSel2/mkSel2"
  forall ts is n0 n1 r. indicesSel2 (mkSel2 ts is n0 n1 r) = is
"elementsSel2_0/mkSel2"
  forall ts is n0 n1 r. elementsSel2_0 (mkSel2 ts is n0 n1 r) = n0
"elementsSel2_1/mkSel2"
  forall ts is n0 n1 r. elementsSel2_1 (mkSel2 ts is n0 n1 r) = n1
"repSel2/mkSel2"
  forall ts is n0 n1 r. repSel2 (mkSel2 ts is n0 n1 r) = r

  #-}


packByTag :: Elt a => Array a -> Array Tag -> Tag -> Array a
{-# INLINE_BACKEND packByTag #-}
packByTag xs tags !tag = fsts (filter (\p -> Prelude.snd p == tag) (zip xs tags))

{-# RULES

"packByTag/bpermute" forall xs is tags n.
  packByTag (bpermute xs is) tags n
    = bpermute xs (packByTag is tags n)

  #-}

{- RULES

"packByTag/combine2ByTag" forall tags1 xs ys tags2 n.
  packByTag (combine2ByTag tags1 xs ys) tags2 n
    = combine2ByTag (packByTag tags1 tags2 n)
                    (packByTag xs (packByTag tags2 tags1 0) n)
                    (packByTag ys (packByTag tags2 tags1 1) n)

  -}

pick :: (Elt a, Eq a) => Array a -> a -> Array Bool
{-# INLINE pick #-}
pick xs !x = map (x==) xs

count :: (Elt a, Eq a) => Array a -> a -> Int
{-# INLINE_BACKEND count #-}
count xs !x = sum (map (tagToInt . fromBool . (==) x) xs)

{-# RULES

"count/seq" forall xs x y. seq (count xs x) y = seq xs (seq x y)

  #-}

count_s :: (Elt a, Eq a) => Segd -> Array a -> a -> Array Int
{-# INLINE_BACKEND count_s #-}
count_s segd xs !x = sum_s segd (map (tagToInt . fromBool . (==) x) xs)

randoms :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
        => Int -> g -> Array a
{-# INLINE_BACKEND randoms #-}

randomRs :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
          => Int -> (a,a) -> g -> Array a
{-# INLINE_BACKEND randomRs #-}


instance IOElt Int
instance IOElt Double
instance (IOElt a, IOElt b) => IOElt (a, b)

hPut :: IOElt a => Handle -> Array a -> IO ()
{-# INLINE_BACKEND hPut #-}

hGet :: IOElt a => Handle -> IO (Array a)
{-# INLINE_BACKEND hGet #-}

toList :: Elt a => Array a -> [a]
{-# INLINE_BACKEND toList #-}

fromList :: Elt a => [a] -> Array a
{-# INLINE_BACKEND fromList #-}

dph_mod_index :: Int -> Int -> Int
{-# INLINE_BACKEND dph_mod_index #-}
dph_mod_index by idx = idx `GHC.Base.remInt` by

dph_plus :: Int -> Int -> Int
{-# INLINE_BACKEND dph_plus #-}
dph_plus x y = x Prelude.+ y

{-# RULES

"dph_plus" forall m n.
  dph_plus (GHC.Base.I# m) (GHC.Base.I# n) = GHC.Base.I# m Prelude.+ GHC.Base.I# n

  #-}

dph_mult :: Int -> Int -> Int
{-# INLINE_BACKEND dph_mult #-}
dph_mult x y = x Prelude.* y

{-# RULES

"bpermute/repeat" forall n len xs is.
  bpermute (repeat n len xs) is
    = len `Prelude.seq` bpermute xs (map (dph_mod_index len) is)

"bpermute/bpermute" forall xs is js.
  bpermute (bpermute xs is) js = bpermute xs (bpermute is js)

  #-}

{-# RULES

"replicate_s/replicate" forall segd k x.
  replicate_s segd (replicate k x) = replicate (elementsSegd segd) x

"replicate_s->replicate_rs" forall n m idxs nm xs.
  replicate_s (mkSegd (replicate n m) idxs nm) xs
    = replicate_rs m xs

"replicate_rs/replicate" forall m n x.
  replicate_rs m (replicate n x) = replicate (m*n) x

"sum/replicate_rs" forall n xs.
  sum (replicate_rs n xs) = sum xs * n

"count/replicate_s" forall segd xs tag.
  count (replicate_s segd xs) tag
    = sum (packByTag (lengthsSegd segd) xs tag)

 #-}

{-# RULES

"repeat/enumFromStepLen[Int]" forall i j k n len.
  repeat n len (enumFromStepLen i j k)
    = generate len (\m -> i + ((m `Prelude.rem` k) * j))

  #-}

{-# RULES

"scan/replicate" forall z n x.
  scan GHC.Base.plusInt z (replicate n x)
    = enumFromStepLen z x n

"map/zipWith (+)/enumFromStepLen" forall m n is.
  map (dph_mod_index m) (zipWith GHC.Base.plusInt (enumFromStepLen 0 m n) is)
    = map (dph_mod_index m) is

 #-}

{-# RULES

"fold_s/replicate1" forall f z n idxs n' xs.
  fold_s f z (mkSegd (replicate n (GHC.Base.I# 1#)) idxs n') xs = xs

"fold_s/replicate" forall f z m n idxs mn xs.
  fold_s f z (mkSegd (replicate m n) idxs mn) xs
    = fold_r f z n xs

  #-}

-- Quickhull rules

tagZeroes :: Array Int -> Array Tag
{-# INLINE CONLIKE PHASE_BACKEND tagZeroes #-}
tagZeroes xs = map (\x -> fromBool (x==0)) xs

{-# RULES

"tagZeroes" forall xs n.
  map fromBool (zipWith GHC.Base.eqInt xs (replicate n (GHC.Base.I# 0#)))
    = tagZeroes xs

"replicate_s/tagZeroes" forall lens idxs n.
  replicate_s (mkSegd lens idxs n) (tagZeroes lens)
    = replicate n 0

"packByTag/replicate" forall xs n t u.
  packByTag xs (replicate n t) u = if t == u then xs else empty

 #-}

{-
"packByTag/tagZeroes" forall xs ys t.
  packByTag xs (tagZeroes ys) t = packByTag xs (map (\y -> fromBool (y==0)) ys) t

"mkSelRep2/tagZeroes" forall xs.
  mkSelRep2 (tagZeroes xs) = 

  -}

{- RULES

"legthsToSegd/replicate" forall m n.
  lengthsToSegd (replicate m n)
    = mkSegd (replicate m n) (enumFromStepLen 0 n m) (m `dph_mult` n)

 -}

-- These are for Gabi
{- RULES

"repeat/bpermute" forall n len xs is.
  repeat n len (bpermute xs is)
    = bpermute xs (repeat n len is)

"lengthsToSegd/replicate" forall m n.
  lengthsToSegd (replicate m n)
    = let { m' = m; n' = n } in toSegd (zip (replicate m' n')
                                            (enumFromStepLen 0 n' m'))

"fromSegd/toSegd" forall ps.
  fromSegd (toSegd ps) = ps

"sum/replicate" forall m n.
  sum (replicate m n) = m Prelude.* n

"replicateEach/zip" forall n lens xs ys.
  replicateEach n lens (zip xs ys)
    = let { n' = n; lens' = lens } in zip (replicateEach n' lens' xs)
                                          (replicateEach n' lens' ys)

"fsts/zip" forall xs ys.
  fsts (zip xs ys) = xs

"snds/zip" forall xs ys.
  snds (zip xs ys) = ys

"repeat/enumFromStepLenEach" forall n m m' ps.
  repeat n m (enumFromStepLenEach m' ps)
    = enumFromStepLenEach (n*m) (repeat n (length ps) ps)

"repeat/zip3" forall n len xs ys zs.
  repeat n len (zip3 xs ys zs)
    = zip3 (repeat n len xs) (repeat n len ys) (repeat n len zs)

"repeat/replicate" forall n len m x.
  repeat n len (replicate m x)
    = replicate len x

 -}

