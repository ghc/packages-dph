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

-- Basics ---------------------------------------------------------------------
-- | O(1). Take the number of elements in an array.
length :: Elt a => Array a -> Int
{-# INLINE_BACKEND length #-}


-- Constructors ---------------------------------------------------------------
-- | An array with no elements.
empty :: Elt a => Array a
{-# INLINE_BACKEND empty #-}


-- | O(n). Append two arrays.
(+:+) :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND (+:+) #-}


-- | Generate a new array given its length and a function to compute each element.
generate :: Elt a => Int -> (Int -> a) -> Array a
{-# INLINE_BACKEND generate #-}
generate n f = map f (enumFromTo 0 (n-1))

generate_cheap :: Elt a => Int -> (Int -> a) -> Array a
{-# INLINE_BACKEND generate_cheap #-}
generate_cheap n f = map f (enumFromTo 0 (n-1))

-- | O(n). Produce a new array by replicating a single element the given number of times.
replicate :: Elt a => Int -> a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate #-}

{-# RULES

"seq/replicate" forall n x y.
  seq (replicate n x) y = n `seq` x `seq` y

 #-}


-- | Produce an array by copying a portion of another array.
repeat  :: Elt a 
        => Int          -- ^ number of times to repeat the source
        -> Int          -- ^ length of source (can be less than the provided array)
        -> Array a      -- ^ array elements to repeat
        -> Array a
{-# INLINE_BACKEND repeat #-}


-- | Tag each element of an array with its index.
--
--   Example: @indexed [:42, 93, 13:] = [:(0, 42), (1, 93), (2, 13):]@ 
indexed :: Elt a => Array a -> Array (Int, a)
{-# INLINE_BACKEND indexed #-}


-- | Generate a range of @Int@s.
enumFromTo :: Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromTo #-}

enumFromThenTo :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromThenTo #-}

enumFromStepLen :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromStepLen #-}

enumFromStepLenEach :: Int -> Array Int -> Array Int -> Array Int -> Array Int
{-# INLINE_BACKEND enumFromStepLenEach #-}


{-# RULES

"repeat/enumFromStepLen[Int]" forall i j k n len.
  repeat n len (enumFromStepLen i j k)
    = generate_cheap len (\m -> i + ((m `Prelude.rem` k) * j))

  #-}


-- Projections ----------------------------------------------------------------
-- | O(1). Retrieve a numbered element from an array.
(!:) :: Elt a => Array a -> Int -> a
{-# INLINE_BACKEND (!:) #-}


-- | O(n). Extract a subrange of elements from an array.
--   Example: @extract [:23, 42, 93, 50, 27:] 1 3  = [:42, 93, 50:]@
extract :: Elt a
        => Array a      -- ^ source array
        -> Int          -- ^ starting index in source array
        -> Int          -- ^ length of result array
        -> Array a
{-# INLINE_BACKEND extract #-}


-- | O(n). Drop some elements from the front of an array, 
--         returning the latter portion.
drop :: Elt a => Int -> Array a -> Array a
{-# INLINE_BACKEND drop #-}


-- Permutation ----------------------------------------------------------------
-- | O(n). Forwards permutation of array elements.
permute :: Elt a 
        => Array a      -- ^ source array
        -> Array Int    -- ^ indices in the destination to copy elements to
        -> Array a
{-# INLINE_BACKEND permute #-}


-- | O(n). Backwards permutation of array elements.
--
--   Example @bpermute [:50, 60, 20, 30:] 3 [:0, 3, 2:]  = [:50, 30, 20:]@
bpermute 
        :: Elt a 
        => Array a      -- ^ source array
        -> Array Int    -- ^ indices in the source to copy elements from.
        -> Array a
{-# INLINE_BACKEND bpermute #-}


-- | Combination of map and bpermute.
--
--   The advantage of using this combined version is that we dont need
--   to apply the parameter function to source elements that dont appear
--   in the result.
mbpermute :: (Elt a, Elt b) => (a->b) -> Array a -> Array Int -> Array b
{-# INLINE_BACKEND mbpermute #-}


-- | Default backwards permutation.
--
--   * The values of the index-value pairs are written into the position in the
--     result array that is indicated by the corresponding index.
--
--   * All positions not covered by the index-value pairs will have the value
--     determined by the initialiser function for that index position.
--
bpermuteDft:: Elt e => Int -> (Int -> e) -> Array (Int, e) -> Array e
{-# INLINE_BACKEND bpermuteDft #-}

{-# RULES
        
"bpermute/repeat" forall n len xs is.
  bpermute (repeat n len xs) is
    = len `Prelude.seq` bpermute xs (map (dph_mod_index len) is)

"bpermute/bpermute" forall xs is js.
  bpermute (bpermute xs is) js = bpermute xs (bpermute is js)

  #-}


-- Update ---------------------------------------------------------------------
-- | O(n). Copy the source array in the destination, using new values for the given indices.
update :: Elt a => Array a -> Array (Int, a) -> Array a
{-# INLINE_BACKEND update #-}


-- Packing and Combining -----------------------------------------------------
-- | Extract the elements from an array that match the given predicate.
filter :: Elt a => (a -> Bool) -> Array a -> Array a
{-# INLINE_BACKEND filter #-}


-- | Extract elements of an array where the associated flag is true.
pack :: Elt a => Array a -> Array Bool -> Array a
{-# INLINE_BACKEND pack #-}


-- | Combine two arrays, using a tag array to tell us where to get each element from.
--
--   Example: @combine [T,F,F,T,T,F] [1,2,3] [4,5,6] = [1,4,5,2,3,6]@
combine :: Elt a => Array Bool -> Array a -> Array a -> Array a
{-# INLINE_BACKEND combine #-}


-- | Like `combine`, but use a precomputed selector to speed up the process.
-- 
--   See dph-prim-seq:"Data.Array.Parallel.Unlifted.Sequential.Segmented.USel"
--   for a description of how this works.
--   
combine2 :: Elt a => Array Tag -> SelRep2 -> Array a -> Array a -> Array a
{-# INLINE_BACKEND combine2 #-}


-- | Interleave the elements of two arrays.
-- 
--   Example: @interleave [1,2,3] [4,5,6] = [1,4,2,5,3,6]@
interleave :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND interleave #-}


-- Zipping and Unzipping ------------------------------------------------------
-- | O(1). Takes two arrays and returns an array of corresponding pairs.
--         If one array is short, excess elements of the longer array are discarded.
zip :: (Elt a, Elt b) => Array a -> Array b -> Array (a, b)
{-# INLINE CONLIKE PHASE_BACKEND zip #-}


-- | O(1). Transform an array into an array of the first components,
--         and an array of the second components.
unzip :: (Elt a, Elt b) => Array (a, b) -> (Array a, Array b)
{-# INLINE_BACKEND unzip #-}


-- | O(1). Take the first elements of an array of pairs.
fsts  :: (Elt a, Elt b) => Array (a, b) -> Array a
{-# INLINE_BACKEND fsts #-}


-- | O(1). Take the second elements of an array of pairs.
snds :: (Elt a, Elt b) => Array (a, b) -> Array b
{-# INLINE_BACKEND snds #-}


-- | O(1). Takes three arrays and returns an array of corresponding triples.
--         If one array is short, excess elements of the longer array are discarded.
zip3 :: (Elt a, Elt b, Elt c) => Array a -> Array b -> Array c -> Array (a, b, c)
{-# INLINE CONLIKE PHASE_BACKEND zip3 #-}


-- | O(1).
unzip3 :: (Elt a, Elt b, Elt c) => Array (a, b, c) -> (Array a, Array b, Array c)
{-# INLINE_BACKEND unzip3 #-}


-- Maps and zipWith -----------------------------------------------------------
-- | Apply a worker function to each element of an array, yielding a new array.
map     :: (Elt a, Elt b)
        => (a -> b) -> Array a -> Array b
{-# INLINE_BACKEND map #-}


-- | zipWith generalises zip by zipping with the function given as the first
--         argument, instead of a tupling function.
zipWith :: (Elt a, Elt b, Elt c)
        => (a -> b -> c) -> Array a -> Array b -> Array c
{-# INLINE_BACKEND zipWith #-}


zipWith3 :: (Elt a, Elt b, Elt c, Elt d)
          => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
{-# INLINE zipWith3 #-}
zipWith3 f xs ys zs
        = zipWith (\(x, y) z -> f x y z)
                  (zip xs ys)
                  zs

zipWith4 :: (Elt a, Elt b, Elt c, Elt d, Elt e)
         => (a -> b -> c -> d -> e)
         -> Array a -> Array b -> Array c -> Array d -> Array e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds
         = zipWith (\(a, b) (c, d) -> f a b c d)
                   (zip as bs)
                   (zip cs ds)


-- Generally useful rules -------------
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


-- FIXME: These are the SMVM rules. They are intentionally quite specific and
-- we want to get rid of the ASAP.

{-# RULES

"map/zipWith (+)/enumFromStepLen" forall m n is.
  map (dph_mod_index m) (zipWith GHC.Base.plusInt (enumFromStepLen 0 m n) is)
    = map (dph_mod_index m) is

"map dph_mod_index/enumFromStepLenEach" forall k l is n1 n2.
  map (dph_mod_index k)
      (enumFromStepLenEach l is (replicate n1 (GHC.Base.I# 1#)) (replicate n2 k))
    = enumFromStepLenEach l (map (dph_mod_index k) is)
                            (replicate n1 (GHC.Base.I# 1#))
                            (replicate n2 k)

"map dph_mod_index/replicate_s" forall k segd xs.
  map (dph_mod_index k) (replicate_s segd xs)
    = replicate_s segd (map (dph_mod_index k) xs)

"map dph_mod_index/enumFromStepLen" forall k# i n.
  map (dph_mod_index (GHC.Base.I# k#)) (enumFromStepLen i (GHC.Base.I# k#) n)
    = replicate n i

"enumFromStepLenEach/replicate x 3" forall k m n1 n2 n3.
  enumFromStepLenEach m (replicate n1 (GHC.Base.I# 0#))
                        (replicate n2 (GHC.Base.I# 1#))
                        (replicate n3 k)
    = generate_cheap m (dph_mod_index k)

"bpermute/generate_cheap" forall n f xs.
  bpermute (generate_cheap n f) xs
    = map f xs
 #-}
              
 
-- The following rules fuse arithmetic operations that shouldnt have been
--  vectorised in the first place. For example, with  z = x * y + a, the vectoriser
--  will lift * and + to vector operations. The result of the  the multiply will be
--  written to a vector, and then read back to do the addition.
--
--  Adding the zipWith rules ensures that the multiply and addition are performed
--  in one go. On the other hand, they can break fusion in the backend library.
--
-- NOTE: These rules are only temporary, they should go away when we have 
--       vectorisation avoidance for scalar operations.

{- RULES  **************** DISABLED

"zipWith/zipWith/zipWith" forall f g h as bs cs ds.
  zipWith f (zipWith g as bs) (zipWith h cs ds)
   = zipWith4 (\a b c d -> f (g a b) (h c d)) as bs cs ds

"zipWith/zipWith_left"  forall f g as bs cs.
  zipWith f (zipWith g as bs) cs
   = zipWith3 (\a b c ->   f (g a b) c) as bs cs

"zipWith/zipWith_right" forall f g as bs cs.
  zipWith f as (zipWith g bs cs)
   = zipWith3 (\a b c ->   f a (g b c)) as bs cs

  -}


-- More rules to recover from the lack of vectorisation avoidance.
-- The regular form of the rules shows why we really dont want to do it this way.

{- RULES  ****************** DISABLED

"map/zipWith" forall f g xs ys.
  map f (zipWith g xs ys)
   = zipWith (\x y -> f (g x y)) xs ys

"zipWith3/map_1" forall f g xs ys zs.
  zipWith3 f (map g xs) ys zs
   = zipWith3 (\x y z -> f (g x) y z) xs ys zs

"zipWith3/map_2" forall f g xs ys zs.
  zipWith3 f xs (map g ys) zs
   = zipWith3 (\x y z -> f x (g y) z) xs ys zs

"zipWith3/map_3" forall f g xs ys zs.
  zipWith3 f xs ys (map g zs)
   = zipWith3 (\x y z -> f x y (g z)) xs ys zs

  -}


-- Folds ----------------------------------------------------------------------

-- | Left fold over an array.
fold :: Elt a => (a -> a -> a) -> a -> Array a -> a
{-# INLINE_BACKEND fold #-}

-- | Left fold over an array, using the first element to initialise the state.
fold1 :: Elt a => (a -> a -> a) -> Array a -> a
{-# INLINE_BACKEND fold1 #-}


-- | Compute the conjunction of all elements in a boolean array.
and :: Array Bool -> Bool
{-# INLINE_BACKEND and #-}

-- | Compute the sum of an array of numbers.
sum :: (Num a, Elt a) => Array a -> a
{-# INLINE_BACKEND sum #-}

-- | Similar to `foldl` but return an array of the intermediate states, including
--   the final state that is computed by `foldl`.
scan :: Elt a => (a -> a -> a) -> a -> Array a -> Array a
{-# INLINE_BACKEND scan #-}


{-# RULES

"seq/sum" forall xs e.
  seq (sum xs) e = seq xs e

"seq/scan<Int> (+)" forall i xs e.
  seq (scan GHC.Base.plusInt i xs) e = i `seq` xs `seq` e

"scan/replicate" forall z n x.
  scan GHC.Base.plusInt z (replicate n x)
    = enumFromStepLen z x n

  #-}


-- Segmented Constructors -----------------------------------------------------
append_s 
        :: Elt a
        => Segd         -- ^ segment descriptor of result aarray
        -> Segd         -- ^ segment descriptor of first array
        -> Array a      -- ^ data of first array
        -> Segd         -- ^ segment descriptor of second array
        -> Array a      -- ^ data of first array
        -> Array a
{-# INLINE_BACKEND append_s #-}


replicate_s :: Elt a => Segd -> Array a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate_s #-}


replicate_rs :: Elt a => Int -> Array a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate_rs #-}


{-# RULES

"append_s->interleave" forall n k idxs1 idxs2 idxs3 m1 m2 m3 xs ys.
  append_s (mkSegd (replicate n k) idxs1 m1)
           (mkSegd (replicate n (GHC.Base.I# 1#)) idxs2 m2) xs
           (mkSegd (replicate n (GHC.Base.I# 1#)) idxs3 m3) ys
    = interleave xs ys

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


-- Segmented Folds ------------------------------------------------------------
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

{-# RULES

"fold_s/replicate1" forall f z n idxs n' xs.
  fold_s f z (mkSegd (replicate n (GHC.Base.I# 1#)) idxs n') xs = xs

"fold_s/replicate" forall f z m n idxs mn xs.
  fold_s f z (mkSegd (replicate m n) idxs mn) xs
    = fold_r f z n xs

  #-}


-- Operations on Segment Descriptors ------------------------------------------
indices_s :: Segd -> Array Int
{-# INLINE_BACKEND indices_s #-}

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


-- Operations on Selectors ----------------------------------------------------

-- | O(1). Construct a selector. Selectors are used to speed up the `combine2` operation.
--
--   See dph-prim-seq:"Data.Array.Parallel.Unlifted.Sequential.Segmented.USel"
--   for a description of how this works.
mkSel2  :: Array Tag            -- ^ tags array
        -> Array Int            -- ^ indices array
        -> Int                  -- ^ number of elements taken from first source array
        -> Int                  -- ^ number of elements taken from second source array
        -> SelRep2      
        -> Sel2
{-# INLINE CONLIKE PHASE_BACKEND mkSel2 #-}


-- | O(1). Get the tags array of a selector.
tagsSel2 :: Sel2 -> Array Tag
{-# INLINE_BACKEND tagsSel2 #-}


-- | O(1). Get the indices array of a selector.
indicesSel2 :: Sel2 -> Array Int
{-# INLINE_BACKEND indicesSel2 #-}


-- | O(1). Get the number of elements that will be taken from the first array.
elementsSel2_0 :: Sel2 -> Int
{-# INLINE_BACKEND elementsSel2_0 #-}


-- | O(1). Get the number of elements that will be taken from the second array.
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


-- | O(n), Compute a selector from a tags array.
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


-- Packing and Picking --------------------------------------------------------

-- | Select the elements of an array that have a corresponding tag.
--   
-- @
-- packByTag [12, 24, 42, 93] [1, 0, 0, 1] 0
--  = [24, 42]
-- @
--
packByTag 
        :: Elt a
        => Array a      -- ^ data values
        -> Array Tag    -- ^ tag values
        -> Tag          -- ^ the tag of values to select
        -> Array a      -- ^ data values that had that tag

{-# INLINE_BACKEND packByTag #-}
packByTag xs tags !tag
        = fsts (filter (\p -> Prelude.snd p == tag) (zip xs tags))


pick :: (Elt a, Eq a) => Array a -> a -> Array Bool
{-# INLINE pick #-}
pick xs !x = map (x==) xs

{-# RULES

"tagZeroes" UNTIL_PHASE_BACKEND forall xs n.
  map fromBool (zipWith GHC.Base.eqInt xs (replicate n (GHC.Base.I# 0#)))
    = tagZeroes xs

"replicate_s/tagZeroes" forall lens idxs n.
  replicate_s (mkSegd lens idxs n) (tagZeroes lens)
    = replicate n 0

"packByTag/replicate" forall xs n t u.
  packByTag xs (replicate n t) u = if t == u then xs else empty

 #-}

{-# RULES

"packByTag/bpermute" forall xs is tags n.
  packByTag (bpermute xs is) tags n
    = bpermute xs (packByTag is tags n)

  #-}




-- Counting -------------------------------------------------------------------
-- | Count the number of elements in array that are equal to the given value.
count :: (Elt a, Eq a) => Array a -> a -> Int
{-# INLINE_BACKEND count #-}
count xs !x = sum (map (tagToInt . fromBool . (==) x) xs)


-- | Count the number of elements in segments that are equal to the given value.
count_s :: (Elt a, Eq a) => Segd -> Array a -> a -> Array Int
{-# INLINE_BACKEND count_s #-}
count_s segd xs !x = sum_s segd (map (tagToInt . fromBool . (==) x) xs)


{-# RULES

"count/seq" forall xs x y. seq (count xs x) y = seq xs (seq x y)

  #-}


-- Random Arrays --------------------------------------------------------------
randoms :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
        => Int -> g -> Array a
{-# INLINE_BACKEND randoms #-}

randomRs :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
          => Int -> (a,a) -> g -> Array a
{-# INLINE_BACKEND randomRs #-}


-- Array IO -------------------------------------------------------------------
instance IOElt Int
instance IOElt Double
instance (IOElt a, IOElt b) => IOElt (a, b)


-- | Write an array to a file.
hPut :: IOElt a => Handle -> Array a -> IO ()
{-# INLINE_BACKEND hPut #-}


-- | Read an array from a file.
hGet :: IOElt a => Handle -> IO (Array a)
{-# INLINE_BACKEND hGet #-}


-- | Convert an array to a list of elements.
toList :: Elt a => Array a -> [a]
{-# INLINE_BACKEND toList #-}


-- | Convert a list of elements to an array.
fromList :: Elt a => [a] -> Array a
{-# INLINE_BACKEND fromList #-}


-- Aliases for primitive operations -------------------------------------------
-- We rename these so we can write rules based on the names, and still
-- control exactly when they get inlined.

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

{- not used 
dph_mult :: Int -> Int -> Int
{-# INLINE_BACKEND dph_mult #-}
dph_mult x y = x Prelude.* y
-}

tagZeroes :: Array Int -> Array Tag
{-# INLINE CONLIKE PHASE_BACKEND tagZeroes #-}
tagZeroes xs = map (\x -> fromBool (x==0)) xs


-------------------------------------------------------------------------------
-- Currently disabled rules
-------------------------------------------------------------------------------

{- RULES

"packByTag/combine2ByTag" forall tags1 xs ys tags2 n.
  packByTag (combine2ByTag tags1 xs ys) tags2 n
    = combine2ByTag (packByTag tags1 tags2 n)
                    (packByTag xs (packByTag tags2 tags1 0) n)
                    (packByTag ys (packByTag tags2 tags1 1) n)

  -}
