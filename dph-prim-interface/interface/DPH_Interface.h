import Data.Array.Parallel.Base ( fromBool )
import qualified GHC.Base
import Prelude ((.), ($), Num(..), Eq(..))

instance Elt Int
instance Elt Word8
instance Elt Bool
instance Elt Double
instance (Elt a, Elt b) => Elt (a :*: b)

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

bpermuteDft:: Elt e => Int -> (Int -> e) -> Array (Int :*: e) -> Array e
{-# INLINE_BACKEND bpermuteDft #-}

update :: Elt a => Array a -> Array (Int :*: a) -> Array a
{-# INLINE_BACKEND update #-}

(+:+) :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND (+:+) #-}


pack :: Elt a => Array a -> Array Bool -> Array a
{-# INLINE_BACKEND pack #-}

combine :: Elt a => Array Bool -> Array a -> Array a -> Array a
{-# INLINE_BACKEND combine #-}

map :: (Elt a, Elt b) => (a -> b) -> Array a -> Array b
{-# INLINE_BACKEND map #-}

filter :: Elt a => (a -> Bool) -> Array a -> Array a
{-# INLINE_BACKEND filter #-}

zip :: (Elt a, Elt b) => Array a -> Array b -> Array (a :*: b)
{-# INLINE CONLIKE PHASE_BACKEND zip #-}

unzip :: (Elt a, Elt b) => Array (a :*: b) -> Array a :*: Array b
{-# INLINE_BACKEND unzip #-}

fsts  :: (Elt a, Elt b) => Array (a :*: b) -> Array a
{-# INLINE_BACKEND fsts #-}

snds :: (Elt a, Elt b) => Array (a :*: b) -> Array b
{-# INLINE_BACKEND snds #-}

zip3 :: (Elt a, Elt b, Elt c) => Array a -> Array b -> Array c
                           -> Array (a :*: b :*: c)
{-# INLINE_BACKEND zip3 #-}

unzip3 :: (Elt a, Elt b, Elt c)
       => Array (a :*: b :*: c) -> Array a :*: Array b :*: Array c
{-# INLINE_BACKEND unzip3 #-}

zipWith :: (Elt a, Elt b, Elt c)
        => (a -> b -> c) -> Array a -> Array b -> Array c
{-# INLINE_BACKEND zipWith #-}

zipWith3 :: (Elt a, Elt b, Elt c, Elt d)
          => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
{-# INLINE_BACKEND zipWith3 #-}


fold :: Elt a => (a -> a -> a) -> a -> Array a -> a
{-# INLINE_BACKEND fold #-}

fold1 :: Elt a => (a -> a -> a) -> Array a -> a
{-# INLINE_BACKEND fold1 #-}

and :: Array Bool -> Bool
{-# INLINE_BACKEND and #-}

sum :: (Num a, Elt a) => Array a -> a
{-# INLINE_BACKEND sum #-}

scan :: Elt a => (a -> a -> a) -> a -> Array a -> Array a
{-# INLINE_BACKEND scan #-}


indexed :: Elt a => Array a -> Array (Int :*: a)
{-# INLINE_BACKEND indexed #-}

enumFromTo :: Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromTo #-}

enumFromThenTo :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromThenTo #-}

enumFromStepLen :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromStepLen #-}

enumFromToEach :: Int -> Array (Int :*: Int) -> Array Int
{-# INLINE_BACKEND enumFromToEach #-}

enumFromStepLenEach :: Int -> Array (Int :*: Int :*: Int) -> Array Int
{-# INLINE_BACKEND enumFromStepLenEach #-}

replicate_s :: Elt a => Segd -> Array a -> Array a
{-# INLINE_BACKEND replicate_s #-}

replicate_rs :: Elt a => Int -> Array a -> Array a
{-# INLINE_BACKEND replicate_rs #-}

append_s :: Elt a => Segd         -- ^ segment descriptor of first array
                  -> Array a      -- ^ data of first array
                  -> Segd         -- ^ segment descriptor of second array
                  -> Array a      -- ^ data of first array
                  -> Array a
{-# INLINE_BACKEND append_s #-}

repeat_c :: Elt a => Int          -- ^ length of the result array
                  -> Array Int    -- ^ number of time a segment is repeated
                  -> Segd         -- ^ segment descriptor
                  -> Array a      -- ^ data array
                  -> Array a
{-# INLINE_BACKEND repeat_c #-}
repeat_c k ns segd xs
  = bpermute xs
  . enumFromStepLenEach k
  $ zip3 (snds ks) (replicate (elementsSegd segd') 1) (fsts ks)
  where
    segd' = lengthsToSegd ns
    ks = replicate_s segd'
       $ zip (lengthsSegd segd) (indicesSegd segd)

fold_s :: Elt a => (a -> a -> a) -> a -> Segd -> Array a -> Array a
{-# INLINE_BACKEND fold_s #-}

fold1_s :: Elt a => (a -> a -> a) -> Segd -> Array a -> Array a
{-# INLINE_BACKEND fold1_s #-}

sum_s :: (Num a, Elt a) => Segd -> Array a -> Array a
{-# INLINE sum_s #-}
sum_s = fold_s (Prelude.+) 0

sum_r :: (Num a, Elt a) => Int -> Int ->Array a -> Array a
{-# INLINE_BACKEND sum_r #-}

indices_s :: Int    -- ^ number of segments
          -> Segd   -- ^ segment descriptor
          -> Int    -- ^ overall number of indices
          -> Array Int
{-# INLINE indices_s #-}
indices_s m segd n = enumFromToEach n
                   . zip (replicate m 0)
                   . map (Prelude.subtract 1)
                   $ lengthsSegd segd

lengthSegd :: Segd -> Int
{-# INLINE_BACKEND lengthSegd #-}

lengthsSegd :: Segd -> Array Int
{-# INLINE_BACKEND lengthsSegd #-}

indicesSegd :: Segd -> Array Int
{-# INLINE_BACKEND indicesSegd #-}

elementsSegd :: Segd -> Int
{-# INLINE_BACKEND elementsSegd #-}

lengthsToSegd :: Array Int -> Segd
{-# INLINE_BACKEND lengthsToSegd #-}

mkSegd :: Array Int -> Array Int -> Int -> Segd
{-# INLINE CONLIKE PHASE_BACKEND mkSegd #-}

{-# RULES

"lengthsSegd/mkSegd" forall lens idxs n.
  lengthsSegd (mkSegd lens idxs n) = lens

"indicesSegd/mkSegd" forall lens idxs n.
  indicesSegd (mkSegd lens idxs n) = idxs

"elementsSegd/mkSegd" forall lens idxs n.
  elementsSegd (mkSegd lens idxs n) = n

 #-}


selectorToIndices2 :: Array Int -> Array Int
{-# INLINE_BACKEND selectorToIndices2 #-}
selectorToIndices2 sel
  = zipWith pick sel
  . scan idx (0 :*: 0)
  $ map start sel
  where
    start 0 = 1 :*: 0
    start _ = 0 :*: 1

    idx (i1 :*: j1) (i2 :*: j2) = (i1+i2 :*: j1+j2)

    pick 0 (i :*: j) = i
    pick _ (i :*: j) = j

pick :: (Elt a, Eq a) => Array a -> a -> Array Bool
{-# INLINE pick #-}
pick xs x = map (x==) xs

count :: (Elt a, Eq a) => Array a -> a -> Int
{-# INLINE_BACKEND count #-}
count xs x = sum (map (fromBool . (==) x) xs)

randoms :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
        => Int -> g -> Array a
{-# INLINE_BACKEND randoms #-}

randomRs :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
          => Int -> (a,a) -> g -> Array a
{-# INLINE_BACKEND randomRs #-}


instance IOElt Int
instance IOElt Double
instance (IOElt a, IOElt b) => IOElt (a :*: b)

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
dph_mod_index by idx = idx `Prelude.mod` by

dph_mult :: Int -> Int -> Int
{-# INLINE_BACKEND dph_mult #-}
dph_mult x y = x Prelude.* y

{-# RULES

"bpermute/repeat" forall n len xs is.
  bpermute (repeat n len xs) is
    = len `Prelude.seq` bpermute xs (map (dph_mod_index len) is)

  #-}

{-# RULES

"replicate_s/replicate" forall segd k x.
  replicate_s segd (replicate k x) = replicate (elementsSegd segd) x

"replicate_s->replicate_rs" forall n m idxs nm xs.
  replicate_s (mkSegd (replicate n m) idxs nm) xs
    = replicate_rs m xs

 #-}

{-# RULES

"repeat_c/repeat" forall k ks n m idxs nm len xs.
  repeat_c k ks (mkSegd (replicate n m) idxs nm) (repeat n len xs)
    = repeat (sum ks) len xs

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

"legthsToSegd/replicate" forall m n.
  lengthsToSegd (replicate m n)
    = mkSegd (replicate m n) (enumFromStepLen 0 n m) (m `dph_mult` n)

 #-}

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

