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

instance Elts Int
instance Elts Word8
instance Elts Float
instance Elts Double

infixl 9 !:
infixr 5 +:+

-- TRAGIC HACKS ===============================================================
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
-- ============================================================================



-- Constructors ===============================================================
-- | O(1). Construct an array with no elements.
empty :: Elt a => Array a
{-# INLINE_BACKEND empty #-}


-- Append -----------------------------
-- | O(n). Append two arrays.
(+:+) :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND (+:+) #-}


-- | Segmented append.
append_s 
        :: Elt a
        => Segd         -- ^ Segment descriptor of result aarray.
        -> Segd         -- ^ Segment descriptor of first array.
        -> Array a      -- ^ Data of first array.
        -> Segd         -- ^ Segment descriptor of second array.
        -> Array a      -- ^ Data of second array.
        -> Array a
{-# INLINE_BACKEND append_s #-}


{-# RULES

"append_s->interleave" forall n k idxs1 idxs2 idxs3 m1 m2 m3 xs ys.
  append_s (mkSegd (replicate n k) idxs1 m1)
           (mkSegd (replicate n (GHC.Base.I# 1#)) idxs2 m2) xs
           (mkSegd (replicate n (GHC.Base.I# 1#)) idxs3 m3) ys
    = interleave xs ys

  #-}


-- Replicate --------------------------
-- | O(n). Construct a new array by replicating a single element the given number of times.
replicate :: Elt a => Int -> a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate #-}

{-# RULES

"seq/replicate" forall n x y.
  seq (replicate n x) y = n `seq` x `seq` y

 #-}


-- | O(n). Segmented replicate.
replicate_s :: Elt a => Segd -> Array a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate_s #-}


-- | O(n). Regular segmented replicate. All segments have the given length.
replicate_rs :: Elt a => Int -> Array a -> Array a
{-# INLINE CONLIKE PHASE_BACKEND replicate_rs #-}


{-# RULES

"replicate_s/replicate" forall segd k x.
  replicate_s segd (replicate k x) = replicate (elementsSegd segd) x

"replicate_s->replicate_rs" forall n m idxs nm xs.
  replicate_s (mkSegd (replicate n m) idxs nm) xs
    = replicate_rs m xs

"replicate_rs/replicate" forall m n x.
  replicate_rs m (replicate n x) = replicate (m*n) x

 #-}



-- Repeat -----------------------------
-- | O(n). Construct an array by copying a portion of another array.
repeat  :: Elt a 
        => Int          -- ^ Number of times to repeat the source.
        -> Int          -- ^ Length of source (can be less than the provided array).
        -> Array a      -- ^ Array elements to repeat.
        -> Array a
{-# INLINE_BACKEND repeat #-}


{-# RULES

"repeat/enumFromStepLen[Int]" forall i j k n len.
  repeat n len (enumFromStepLen i j k)
    = generate_cheap len (\m -> i + ((m `Prelude.rem` k) * j))

  #-}


-- Indexed ----------------------------
-- | O(n). Tag each element of an array with its index.
--
--   @indexed [42, 93, 13] = [(0, 42), (1, 93), (2, 13)]@ 
indexed :: Elt a => Array a -> Array (Int, a)
{-# INLINE_BACKEND indexed #-}


-- Generate ---------------------------
-- | Generate a new array given its length and a function to compute each element.
generate :: Elt a => Int -> (Int -> a) -> Array a
{-# INLINE_BACKEND generate #-}
generate n f = map f (enumFromTo 0 (n-1))

generate_cheap :: Elt a => Int -> (Int -> a) -> Array a
{-# INLINE_BACKEND generate_cheap #-}
generate_cheap n f = map f (enumFromTo 0 (n-1))


-- Indices ----------------------------
-- | Segmented indices. Fill an array with [0..len-1] for segment.
indices_s :: Segd -> Array Int
{-# INLINE_BACKEND indices_s #-}


-- Enumerations -----------------------
enumFromTo :: Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromTo #-}

enumFromThenTo :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromThenTo #-}

enumFromStepLen :: Int -> Int -> Int -> Array Int
{-# INLINE_BACKEND enumFromStepLen #-}

enumFromStepLenEach :: Int -> Array Int -> Array Int -> Array Int -> Array Int
{-# INLINE_BACKEND enumFromStepLenEach #-}


-- Projections ================================================================
-- | O(1). Take the number of elements in an array.
length :: Elt a => Array a -> Int
{-# INLINE_BACKEND length #-}


-- | O(1). Retrieve a numbered element from an array.
(!:) :: Elt a => Array a -> Int -> a
{-# INLINE_BACKEND (!:) #-}


-- | O(1). Retrieve a numbered element from an array, without bounds checks.
unsafeIndex :: Elt a => Array a -> Int -> a
{-# INLINE_BACKEND unsafeIndex #-}


-- | O(n). Extract a subrange of elements from an array.
--  
--   @extract [23, 42, 93, 50, 27] 1 3  = [42, 93, 50]@
extract :: Elt a
        => Array a      -- ^ Source array.
        -> Int          -- ^ Starting index in source array.
        -> Int          -- ^ Length of result array.
        -> Array a
{-# INLINE_BACKEND extract #-}


-- | O(n). Segmented extract, from a vector of arrays.
--   TODO: This is a transitory interface, we are refactoring code to always
--         use the previous form.
unsafeExtracts_nss
        :: Elt a
        => SSegd
        -> VV.Vector (Array a)
        -> Array a
{-# INLINE_BACKEND unsafeExtracts_nss #-}


-- | O(n). Segmented extract.
unsafeExtracts_ass
        :: (Elt a, Elts a)
        => SSegd        -- ^ `SSegd` defining the slices to extract.
        -> Arrays a     -- ^ Source arrays.
        -> Array a
{-# INLINE_BACKEND unsafeExtracts_ass #-}


-- | O(n). Segmented extract.
unsafeExtracts_avs
        :: (Elt a, Elts a)
        => VSegd        -- ^ `VSegd` defining the slices to extract.
        -> Arrays a     -- ^ Source arrays.
        -> Array a
{-# INLINE_BACKEND unsafeExtracts_avs #-}


-- | O(n). Drop some elements from the front of an array, 
--         returning the latter portion.
drop :: Elt a => Int -> Array a -> Array a
{-# INLINE_BACKEND drop #-}


-- Update =====================================================================
-- | O(n). Copy the source array in the destination, using new values for the given indices.
update :: Elt a => Array a -> Array (Int, a) -> Array a
{-# INLINE_BACKEND update #-}


-- Permutation ================================================================
-- | O(n). Forwards permutation of array elements.
permute :: Elt a 
        => Array a      -- ^ Source array.
        -> Array Int    -- ^ Indices in the destination to copy elements to.
        -> Array a
{-# INLINE_BACKEND permute #-}


-- | O(n). Backwards permutation of array elements.
--
--   @bpermute [50, 60, 20, 30] 3 [0, 3, 2]  = [50, 30, 20]@
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
--   The values of the index-value pairs are written into the position in the
--   result array that is indicated by the corresponding index.
--
--   All positions not covered by the index-value pairs will have the value
--   determined by the initialiser function for that index position.
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


-- Zipping and Unzipping ======================================================
-- | O(1). Zip two arrays into an array of pairs.
--         If one array is short, excess elements of the longer array are discarded.
zip :: (Elt a, Elt b) => Array a -> Array b -> Array (a, b)
{-# INLINE CONLIKE PHASE_BACKEND zip #-}


-- | O(1). Zip three arrays into an array of triples.
--         If one array is short, excess elements of the longer arrays are discarded.
zip3 :: (Elt a, Elt b, Elt c) => Array a -> Array b -> Array c -> Array (a, b, c)
{-# INLINE CONLIKE PHASE_BACKEND zip3 #-}


-- | O(1). Unzip an array of pairs into a pair of arrays.
unzip :: (Elt a, Elt b) => Array (a, b) -> (Array a, Array b)
{-# INLINE_BACKEND unzip #-}


-- | O(1). Unzip an array of triples into a triple of arrays.
unzip3 :: (Elt a, Elt b, Elt c) => Array (a, b, c) -> (Array a, Array b, Array c)
{-# INLINE_BACKEND unzip3 #-}


-- | O(1). Take the first elements of an array of pairs.
fsts  :: (Elt a, Elt b) => Array (a, b) -> Array a
{-# INLINE_BACKEND fsts #-}


-- | O(1). Take the second elements of an array of pairs.
snds :: (Elt a, Elt b) => Array (a, b) -> Array b
{-# INLINE_BACKEND snds #-}


-- Maps and zipWith ===========================================================
-- | Apply a worker function to each element of an array, yielding a new array.
map     :: (Elt a, Elt b)
        => (a -> b) -> Array a -> Array b
{-# INLINE_BACKEND map #-}


-- | Apply a worker function to correponding elements of two arrays.
zipWith :: (Elt a, Elt b, Elt c)
        => (a -> b -> c) -> Array a -> Array b -> Array c
{-# INLINE_BACKEND zipWith #-}


-- | Apply a worker function to corresponding elements of three arrays.
zipWith3 :: (Elt a, Elt b, Elt c, Elt d)
          => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
{-# INLINE zipWith3 #-}
zipWith3 f xs ys zs
        = zipWith (\(x, y) z -> f x y z)
                  (zip xs ys)
                  zs

-- | Apply a worker function to corresponding elements of four arrays.
zipWith4 :: (Elt a, Elt b, Elt c, Elt d, Elt e)
         => (a -> b -> c -> d -> e)
         -> Array a -> Array b -> Array c -> Array d -> Array e
{-# INLINE zipWith4 #-}
zipWith4 f as bs cs ds
         = zipWith (\(a, b) (c, d) -> f a b c d)
                   (zip as bs)
                   (zip cs ds)


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



-- Folds and Scans ============================================================

-- Fold -------------------------------
-- | Undirected fold over an array.
--
--   * The worker function must be associative.
--
--   * The provided starting element must be neutral with respect to the worker.
--     For example 0 is neutral wrt (+) and 1 is neutral wrt (*).
--
fold :: Elt a => (a -> a -> a) -> a -> Array a -> a
{-# INLINE_BACKEND fold #-}

-- | Undirected segmented fold.
--   Same preconditions as `fold`.
fold_s :: Elt a => (a -> a -> a) -> a -> Segd -> Array a -> Array a
{-# INLINE_BACKEND fold_s #-}

-- | Undirected scattered segmented fold.
--   Same preconditions as `fold`.
fold_ss :: (Elts a, Elt a)
        => (a -> a -> a) -> a -> SSegd -> Arrays a -> Array a
{-# INLINE_BACKEND fold_ss #-}

-- | Regular segmented fold. All segements have the given length.
--   Same preconditions as `fold`.
fold_r :: Elt a => (a -> a -> a) -> a -> Int -> Array a -> Array a
{-# INLINE_BACKEND fold_r #-}


-- Fold1 -------------------------------
-- | Undirected fold,
--   using the first element to initialise the state.
--   If the array contains no elements then you'll get a bounds check `error`.
--   Same preconditions as `fold`.
fold1 :: Elt a => (a -> a -> a) -> Array a -> a
{-# INLINE_BACKEND fold1 #-}

-- | Undirected segmented fold,
--   using the first element of each segment to initialise the state of that segment.
--   Same preconditions as `fold`.
fold1_s :: Elt a => (a -> a -> a) -> Segd -> Array a -> Array a
{-# INLINE_BACKEND fold1_s #-}

-- | Undirected scattered segmented fold over an array, 
--   using the first element of each segment to initialise the state of that segment.
--   Same preconditions as `fold`.
fold1_ss :: (Elts a, Elt a) 
         => (a -> a -> a) -> SSegd -> Arrays a -> Array a
{-# INLINE_BACKEND fold1_ss #-}


-- Sums -------------------------------
-- | Sum the elements of an array.
sum :: (Num a, Elt a) => Array a -> a
{-# INLINE_BACKEND sum #-}

-- | Segmented sum.
sum_s :: (Num a, Elt a) => Segd -> Array a -> Array a
sum_s = fold_s (Prelude.+) 0
{-# INLINE sum_s #-}

-- | Scattered segmented sum.
sum_ss :: (Num a, Elts a, Elt a) 
       => SSegd -> Arrays a -> Array a
sum_ss = fold_ss (Prelude.+) 0
{-# INLINE sum_ss #-}

-- | Regular segmented sum. All segments have the given length.
sum_r :: (Num a, Elt a) => Int -> Array a -> Array a
{-# INLINE_BACKEND sum_r #-}


-- Count ------------------------------
-- | Count the number of elements in array that are equal to the given value.
count :: (Elt a, Eq a) => Array a -> a -> Int
count xs !x = sum (map (tagToInt . fromBool . (==) x) xs)
{-# INLINE_BACKEND count #-}


-- | Segmented count.
count_s :: (Elt a, Eq a) => Segd -> Array a -> a -> Array Int
count_s segd xs !x
        = sum_s segd (map (tagToInt . fromBool . (==) x) xs)
{-# INLINE_BACKEND count_s #-}


-- | Scattered segmented count.

--   TODO: Make this take Arrays directly.
count_ss :: (Elt a, Eq a) => SSegd -> VV.Vector (Array a) -> a -> Array Int
{-# INLINE_BACKEND count_ss #-}
count_ss ssegd xs !x
        = sum_ss ssegd (fromVectors $ VV.map (map (tagToInt . fromBool . (==) x)) xs)


-- And --------------------------------
-- | Compute the conjunction of all elements in a boolean array.
and :: Array Bool -> Bool
{-# INLINE_BACKEND and #-}


-- Scans ------------------------------
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

"sum/replicate_rs" forall n xs.
  sum (replicate_rs n xs) = sum xs * n

"count/replicate_s" forall segd xs tag.
  count (replicate_s segd xs) tag
    = sum (packByTag (lengthsSegd segd) xs tag)

"fold_s/replicate1" forall f z n idxs n' xs.
  fold_s f z (mkSegd (replicate n (GHC.Base.I# 1#)) idxs n') xs = xs

"fold_s/replicate" forall f z m n idxs mn xs.
  fold_s f z (mkSegd (replicate m n) idxs mn) xs
    = fold_r f z n xs

"count/seq" forall xs x y. seq (count xs x) y = seq xs (seq x y)

  #-}


-- Pack and Filter ============================================================
-- | Extract elements of an array where the associated flag is true.
pack :: Elt a => Array a -> Array Bool -> Array a
{-# INLINE_BACKEND pack #-}


-- | Select the elements of an array that have a corresponding tag.
--   
--   @packByTag [12, 24, 42, 93] [1, 0, 0, 1] 0 = [24, 42]@
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


-- | Extract the elements from an array that match the given predicate.
filter :: Elt a => (a -> Bool) -> Array a -> Array a
{-# INLINE_BACKEND filter #-}

-- | Compute an array of flags indicating which elements match a given value.
--
--   @pick [4, 5, 3, 6, 5, 2, 5] 5 = [F, T, F, F, T, F, T]@
pick :: (Elt a, Eq a) => Array a -> a -> Array Bool
{-# INLINE pick #-}
pick xs !x = map (x ==) xs


{-# RULES

"tagZeroes" UNTIL_PHASE_BACKEND forall xs n.
  map fromBool (zipWith GHC.Base.eqInt xs (replicate n (GHC.Base.I# 0#)))
    = tagZeroes xs

"replicate_s/tagZeroes" forall lens idxs n.
  replicate_s (mkSegd lens idxs n) (tagZeroes lens)
    = replicate n 0

"packByTag/replicate" forall xs n t u.
  packByTag xs (replicate n t) u = if t == u then xs else empty

"packByTag/bpermute" forall xs is tags n.
  packByTag (bpermute xs is) tags n
    = bpermute xs (packByTag is tags n)

  #-}


-- Combine and Interleave =====================================================
-- | Combine two arrays, using a tag array to tell us where to get each element from.
--
--   @combine [T, F, F, T, T, F] [1, 2, 3] [4, 5, 6] = [1, 4, 5, 2, 3, 6]@
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
--    @interleave [1, 2, 3] [4, 5, 6] = [1, 4, 2, 5, 3, 6]@
interleave :: Elt a => Array a -> Array a -> Array a
{-# INLINE_BACKEND interleave #-}


-- Selectors ==================================================================
-- | O(1). Construct a selector.
mkSel2  :: Array Tag            -- ^ Tags array.
        -> Array Int            -- ^ Indices array.
        -> Int                  -- ^ Number of elements taken from first source array.
        -> Int                  -- ^ Number of elements taken from second source array.
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


-- | O(1). Take the `SelRep2` from a `Sel2`.
repSel2 :: Sel2 -> SelRep2
{-# INLINE_BACKEND repSel2 #-}


-- | O(n). Construct a `SelRep2`.
mkSelRep2 :: Array Tag -> SelRep2
{-# INLINE CONLIKE PHASE_BACKEND mkSelRep2 #-}


-- | O(n). 
indicesSelRep2 :: Array Tag -> SelRep2 -> Array Int
{-# INLINE_BACKEND indicesSelRep2 #-}


-- | O(n). Count the number of elements to take from the first array.
elementsSelRep2_0 :: Array Tag -> SelRep2 -> Int
{-# INLINE_BACKEND elementsSelRep2_0 #-}


-- | O(n). Count the number of elements to take from the second array.
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


-- Segment Descriptors ========================================================
-- | O(max(segs, threads) . log segs). Construct a segment desciprtor from an
--   array of segment lengths, starting indices, and total number of elements.
--
--   A segment desciptor defines an irregular 2D array based on a flat, 1D array
--   of elements. 
-- 
--   The defined array is an array of segments, where every segment covers
--   some of the elements from the flat array.
-- 
--   /INVARIANT:/ The segment starting indices must be monotonically increasing,
--   and all elements from the flat array must be covered by some segment.
--   This is because, in the implementation of `lengthsToSegd`, we binary
--   search the indices to determine which segment an element of the 
--   flat array belongs to. It also means that the segment indices can always
--   be reconstructed from the segment lengths by `lengthsToSegd`.
--
mkSegd :: Array Int -> Array Int -> Int -> Segd
{-# INLINE CONLIKE PHASE_BACKEND mkSegd #-}


-- | Check whether a `Segd` is well formed.
--
--   * This will only return `False` if there is a bug in the library.
validSegd :: Segd -> Bool
{-# NOINLINE validSegd #-}
--  NOINLINE because it's only used during debugging.


-- | O(1). Construct an empty `Segd`.
emptySegd :: Segd
{-# INLINE_BACKEND emptySegd #-}


-- | O(1). Construct a `Segd` containing a single segment of the given length.
singletonSegd :: Int -> Segd
{-# INLINE_BACKEND singletonSegd #-}


-- | O(max(segs,threads) . log segs). Construct a `Segd` from an array of segment lengths.
lengthsToSegd :: Array Int -> Segd
lengthsToSegd ns = mkSegd ns (scan (+) 0 ns) (sum ns)
{-# INLINE_BACKEND lengthsToSegd #-}


-- | O(1). Take the length of a `Segd`.
lengthSegd :: Segd -> Int
{-# INLINE_BACKEND lengthSegd #-}


-- | O(1). Take the segment lengths of a `Segd`.
lengthsSegd :: Segd -> Array Int
{-# INLINE_BACKEND lengthsSegd #-}


-- | O(1). Take the segment starting indices of a `Segd`.
indicesSegd :: Segd -> Array Int
{-# INLINE_BACKEND indicesSegd #-}


-- | O(1). Take the total number of elements defined by a `Segd`.
elementsSegd :: Segd -> Int
{-# INLINE_BACKEND elementsSegd #-}

-- | O(max(segs,threads) . log segs). Add the lengths of corresponding segments in two descriptors.
--
--   @plusSegd [lens: 2 3 1] [lens: 3 1 1] = [lens: 5 4 2]@
--    
plusSegd :: Segd -> Segd -> Segd
plusSegd segd1 segd2
  = mkSegd (zipWith (+) (lengthsSegd segd1) (lengthsSegd segd2))
           (zipWith (+) (indicesSegd segd1) (indicesSegd segd2))
           (elementsSegd segd1 `dph_plus` elementsSegd segd2)
{-# INLINE_BACKEND plusSegd #-}


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

-- Scattered Segment Descriptors ==============================================
-- | Construct a Scattered Segment Descriptor from an array of source array 
--   indices, starting indices and an existing `Segd`.
--
--   * A `SSegd` is an extension of a `Segd` that that allows the segments to be
--     scattered through multiple flat arrays.
--
--   * Each segment is associated with a source id that indicates what 
--     flat array it is in, along with the starting index in that flat array.
--
--   * The segments need not cover the entire flat array.
--
--   * Different segments may point to the same elements.
--
--   * As different segments may point to the same elements, it is possible
--     for the total number of elements covered by the segment descriptor
--     to overflow a machine word.
--
mkSSegd :: Array Int -> Array Int -> Segd -> SSegd
{-# INLINE CONLIKE PHASE_BACKEND mkSSegd #-}


-- | Check whether a `Segd` is well formed.
--
--   * This will only return `False` if there is a bug in the library.
validSSegd :: SSegd -> Bool
{-# NOINLINE validSSegd #-}
--  NOINLINE because it's only used during debugging.


-- | O(1). Construct an empty `SSegd`.
emptySSegd :: SSegd
{-# INLINE_BACKEND emptySSegd #-}


-- | O(1). Construct a `Segd` containing a single segment of the given length.
singletonSSegd :: Int -> SSegd
{-# INLINE_BACKEND singletonSSegd #-}


-- | O(max(segs,threads) . log segs). Promote a `Segd` to a `SSegd`, 
--   assuming all segments are contiguous and come from a single array.
promoteSegdToSSegd :: Segd -> SSegd
{-# INLINE_BACKEND promoteSegdToSSegd #-}


-- | O(1). True when the segment starts are identical to the segment indices
--   field and the sources are all 0's. 
--
--   In this case all the data elements are in one contiguous flat
--   array, and consumers can avoid looking at the real starts and
--   sources fields.
isContiguousSSegd :: SSegd -> Bool
{-# INLINE_BACKEND isContiguousSSegd #-}


-- | O(1). Take the length of a `SSegd`.
lengthOfSSegd :: SSegd -> Int
{-# INLINE_BACKEND lengthOfSSegd #-}


-- | O(1). Take the segment lengths of a `SSegd`.
lengthsOfSSegd :: SSegd -> Array Int
{-# INLINE_BACKEND lengthsOfSSegd #-}


-- | O(1). Take the segment starting indices of a `SSegd`.
indicesOfSSegd :: SSegd -> Array Int
{-# INLINE_BACKEND indicesOfSSegd #-}


-- | O(1). Take the segment starting indices of a `SSegd`.
startsOfSSegd :: SSegd -> Array Int
{-# INLINE_BACKEND startsOfSSegd #-}


-- | O(1). Take the source ids of a `SSegd`.
sourcesOfSSegd :: SSegd -> Array Int
{-# INLINE_BACKEND sourcesOfSSegd #-}


-- | O(1). Get the length, segment index, starting index, and source id of a segment.
getSegOfSSegd :: SSegd -> Int -> (Int, Int, Int, Int)
{-# INLINE_BACKEND getSegOfSSegd #-}


-- | Produce a segment descriptor that describes the result of appending two
--   segmented arrays.
appendSSegd :: SSegd -> Int -> SSegd -> Int -> SSegd
{-# INLINE_BACKEND appendSSegd #-}
        

-- Virtual Segment descriptors ================================================
-- | Construct a Virtual Segment Descriptor from an array of virtual segment
--   ids and an existing `SSegd`.
--
--   A `VSegd` is an extension of a `SSegd` that allows data from the underlying
--   flat array to be shared between segments. For example, you can define an array
--   of 10 virtual segments that all have the same length and elements as a
--   single physical segment.
--
--   Internally, we maintain the invariant that all physical segments are 
--   reachable by some virtual segment. This is needed to ensure that operations
--   such as segmented fold have the right complexity. If you don't need the 
--   invariant then you can sidestep the code that maintains it by using the
--   redundant versions of the following operators.
-- 
mkVSegd :: Array Int -> SSegd -> VSegd
{-# INLINE_BACKEND mkVSegd #-}


-- | Check whether a `Segd` is well formed.
--
--   * This will only return `False` if there is a bug in the library.
validVSegd :: VSegd -> Bool
{-# INLINE_BACKEND validVSegd #-}


-- | O(1). Construct an empty `SSegd`.
emptyVSegd :: VSegd
{-# INLINE_BACKEND emptyVSegd #-}


-- | O(1). Construct a `VSegd` containing a single segment of the given length.
singletonVSegd :: Int -> VSegd
{-# INLINE_BACKEND singletonVSegd #-}


-- | O(segs). Promote a plain `Segd` to a `VSegd`.
--   The result contains one virtual segment for every physical segment
--   the provided `Segd`.
promoteSegdToVSegd :: Segd -> VSegd
{-# INLINE_BACKEND promoteSegdToVSegd #-}


-- | O(segs). Promote a plain `SSegd` to a `VSegd`.
--   The result contains one virtual segment for every physical segment
--   the provided `SSegd`.
promoteSSegdToVSegd :: SSegd -> VSegd
{-# INLINE_BACKEND promoteSSegdToVSegd #-}


-- | O(1). Checks whether all the segments are manifest (unshared / non-virtual).
--   If this is the case, then the vsegids field will be [0..len-1]. 
--
--   Consumers can check this field to avoid demanding the vsegids field.
--   This can avoid the need for it to be constructed in the first place, due to
--   lazy evaluation.
isManifestVSegd :: VSegd -> Bool
{-# INLINE_BACKEND isManifestVSegd #-}


-- | O(1). Checks whether the starts are identical to the indices field and
--   the sourceids are all 0's. 
--
--   In this case all the data elements are in one contiguous flat array, and
--   consumers can avoid looking at the real starts and sources fields.
isContiguousVSegd :: VSegd -> Bool
{-# INLINE_BACKEND isContiguousVSegd #-}


-- | O(1). Take the length of a `VSegd`.
lengthOfVSegd :: VSegd -> Int
{-# INLINE_BACKEND lengthOfVSegd #-}


-- | O(1). Take the vsegids of a `VSegd`.
takeVSegidsOfVSegd :: VSegd -> Array Int
{-# INLINE_BACKEND takeVSegidsOfVSegd #-}


-- | O(1). Take the vsegids of a `VSegd`, but don't require that every physical
--   segment is referenced by some virtual segment.
--
--   If you're just performing indexing and don't need the invariant that all
--   physical segments are reachable from some virtual segment, then use this
--   version as it's faster. This sidesteps the code that maintains the invariant.
--
--   The stated O(1) complexity assumes that the array has already been fully
--   evalauted. If this is not the case then we can avoid demanding the result
--   of a prior computation on the vsegids, thus reducing the cost attributed
--   to that prior computation.
--
takeVSegidsRedundantOfVSegd :: VSegd -> Array Int
{-# INLINE_BACKEND takeVSegidsRedundantOfVSegd #-}


-- | O(1). Take the `SSegd` of a `VSegd`.
takeSSegdOfVSegd :: VSegd -> SSegd
{-# INLINE_BACKEND takeSSegdOfVSegd #-}


-- | O(1). Take the `SSegd` of a `VSegd`, but don't require that every physical
--   segment is referenced by some virtual segment.
--
--   See the note in `takeVSegidsRedundantOfVSegd`.
takeSSegdRedundantOfVSegd :: VSegd -> SSegd
{-# INLINE_BACKEND takeSSegdRedundantOfVSegd #-}


-- | O(1). Take the segment lengths of a `VSegd`.
takeLengthsOfVSegd :: VSegd -> Array Int
{-# INLINE_BACKEND takeLengthsOfVSegd #-}


-- | O(1). Get the length, starting index, and source id of a segment.
getSegOfVSegd :: VSegd -> Int -> (Int, Int, Int)
{-# INLINE_BACKEND getSegOfVSegd #-}


-- | O(segs). Yield a `SSegd` that describes each segment of a `VSegd`
--   individually.
--
--   By doing this we lose information about which virtual segments
--   correspond to the same physical segments.
demoteToSSegdOfVSegd :: VSegd -> SSegd
{-# INLINE_BACKEND demoteToSSegdOfVSegd #-}


-- | O(segs). Yield a `Segd` that describes each segment of a `VSegd`
--   individually, assuming all segments have been concatenated to 
--   remove scattering.
--
--   /WARNING/: Trying to take the `Segd` of a nested array that has been
--   constructed with replication can cause index space overflow. This is
--   because the virtual size of the corresponding flat data can be larger
--   than physical memory. If this happens then indices fields and 
--   element count in the result will be invalid.
--
unsafeDemoteToSegdOfVSegd :: VSegd -> Segd
{-# INLINE_BACKEND unsafeDemoteToSegdOfVSegd #-}


-- | Update the vsegids of a `VSegd`, and then cull the physical
--   segment descriptor so that all physical segments are reachable from
--   some virtual segment.
--
updateVSegsOfVSegd :: (Array Int -> Array Int) -> VSegd -> VSegd
{-# INLINE_BACKEND updateVSegsOfVSegd #-}


-- | Update the vsegids  of `VSegd`, where the result is guaranteed to
--   cover all physical segments.
--
--   Using this version saves performing the 'cull' operation which 
--   discards unreachable physical segments.
--
--   * The resulting vsegids must cover all physical segments.
--     If they do not then there will be physical segments that are not 
--     reachable from some virtual segment, and subsequent operations
--     like segmented fold will have the wrong work complexity.
--
updateVSegsReachableOfVSegd :: (Array Int -> Array Int) -> VSegd -> VSegd
{-# INLINE_BACKEND updateVSegsReachableOfVSegd #-}


-- | Produce a virtual segment descriptor that describes the result of 
--   appending two segmented arrays.
appendVSegd 
        :: VSegd        -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> VSegd        -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> VSegd
{-# INLINE_BACKEND appendVSegd #-}


-- | Combine two virtual segment descriptors.
combine2VSegd
        :: Sel2         -- ^ Selector for the combine operation.
        -> VSegd        -- ^ Descriptor of first array.
        -> Int          -- ^ Number of flat physical arrays for first descriptor.
        -> VSegd        -- ^ Descriptor of second array.
        -> Int          -- ^ Number of flat physical arrays for second descriptor.
        -> VSegd
{-# INLINE_BACKEND combine2VSegd #-}


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

tagZeroes :: Array Int -> Array Tag
{-# INLINE CONLIKE PHASE_BACKEND tagZeroes #-}
tagZeroes xs = map (\x -> fromBool (x==0)) xs

