instance Elt Int
instance Elt Word8
instance Elt Bool
instance Elt Double
instance (Elt a, Elt b) => Elt (a :*: b)

infixl 9 !:
infixr 5 +:+
infixr 5 ^+:+^
infixr 9 >:

length :: Elt a => Array a -> Int
{-# INLINE_BACKEND length #-}

empty :: Elt a => Array a
{-# INLINE_BACKEND empty #-}

replicate :: Elt a => Int -> a -> Array a
{-# INLINE_BACKEND replicate #-}

replicateEach :: Elt a => Int -> Array Int -> Array a -> Array a
{-# INLINE_BACKEND replicateEach #-}

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
{-# INLINE_BACKEND zip #-}

unzip :: (Elt a, Elt b) => Array (a :*: b) -> Array a :*: Array b
{-# INLINE_BACKEND unzip #-}

fsts  :: (Elt a, Elt b) => Array (a :*: b) -> Array a
{-# INLINE_BACKEND fsts #-}

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


concat :: Elt a => SArray a -> Array a
{-# INLINE_BACKEND concat #-}

(>:) :: Elt a => Segd -> Array a -> SArray a
{-# INLINE_BACKEND (>:) #-}

(^+:+^) :: Elt a => SArray a -> SArray a -> SArray a
{-# INLINE_BACKEND (^+:+^) #-}


length_s :: Elt a => SArray a -> Int
{-# INLINE_BACKEND length_s #-}

lengths_s :: Elt a => SArray a -> Array Int
{-# INLINE_BACKEND lengths_s #-}

replicate_s :: Elt a => Segd -> Array a -> SArray a
{-# INLINE_BACKEND replicate_s #-}

repeat_c :: Elt a => Int -> Array Int -> Segd -> Array a -> Array a
{-# INLINE_BACKEND repeat_c #-}

indices_s :: Elt a => SArray a -> Array Int
{-# INLINE_BACKEND indices_s #-}


fst_s :: (Elt a, Elt b) => SArray (a :*: b) -> SArray a
{-# INLINE_BACKEND fst_s #-}

snd_s :: (Elt a, Elt b) => SArray (a :*: b) -> SArray b
{-# INLINE_BACKEND snd_s #-}

zip_s :: (Elt a, Elt b) => SArray a -> SArray b -> SArray (a :*: b)
{-# INLINE_BACKEND zip_s #-}


bpermute_s' :: Elt a => Array a -> SArray Int -> SArray a
{-# INLINE_BACKEND bpermute_s' #-}


map_s:: (Elt a, Elt b) => (a -> b) -> SArray a -> SArray b
{-# INLINE_BACKEND map_s #-}

filter_s :: Elt a => (a -> Bool) -> SArray a -> SArray a
{-# INLINE_BACKEND filter_s #-}

pack_c :: Elt a => Array Bool -> SArray a -> SArray a
{-# INLINE_BACKEND pack_c #-}

combine_c :: Elt a => Array Bool -> SArray a -> SArray a -> SArray a
{-# INLINE_BACKEND combine_c #-}

zipWith_s :: (Elt a, Elt b, Elt c) 
          => (a -> b -> c) -> SArray a -> SArray b -> SArray c
{-# INLINE_BACKEND zipWith_s #-}


fold_s :: Elt a => (a -> a -> a) -> a -> SArray a -> Array a
{-# INLINE_BACKEND fold_s #-}

fold1_s :: Elt a => (a -> a -> a) -> SArray a -> Array a
{-# INLINE_BACKEND fold1_s #-}

sum_s :: (Num a, Elt a) => SArray a -> Array a
{-# INLINE_BACKEND sum_s #-}


enumFromThenTo_s :: Array Int -> Array Int -> Array Int -> SArray Int
{-# INLINE_BACKEND enumFromThenTo_s #-}


indexed_s :: Elt a => SArray a -> SArray (Int :*: a)
{-# INLINE_BACKEND indexed_s #-}


lengthsSegd :: Segd -> Array Int
{-# INLINE_BACKEND lengthsSegd #-}

lengthsToSegd :: Array Int -> Segd
{-# INLINE_BACKEND lengthsToSegd #-}

toSegd :: Array (Int :*: Int) -> Segd
{-# INLINE_BACKEND toSegd #-}

fromSegd :: Segd -> Array (Int :*: Int)
{-# INLINE_BACKEND fromSegd #-}


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

toList_s :: Elt a => SArray a -> [[a]]
{-# INLINE_BACKEND toList_s #-}

fromList_s :: Elt a => [[a]] -> SArray a
{-# INLINE_BACKEND fromList_s #-}

{-# RULES

"bpermute/repeat" forall n xs is.
  bpermute (repeat n xs) is
    = let k = length xs in k `Prelude.seq` bpermute xs (map (`Prelude.mod` k) is)

  #-}

-- FIXME
-- WARNING: THIS RULE IS UTTERLY WRONG, it's only here for smvm which we can't
-- really optimise at the moment.
--
{-# RULES

"repeat_c/repeat" forall n ns segd k xs.
  repeat_c n ns segd (repeat k xs)
    = repeat (k Prelude.* sum ns) xs
  #-}

