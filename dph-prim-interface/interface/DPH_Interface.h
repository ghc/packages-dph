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
{-# INLINE length #-}

empty :: Elt a => Array a
{-# INLINE empty #-}

replicate :: Elt a => Int -> a -> Array a
{-# INLINE replicate #-}

replicateEach :: Elt a => Int -> Array Int -> Array a -> Array a
{-# INLINE replicateEach #-}

repeat :: Elt a => Int -> Array a -> Array a
{-# INLINE repeat #-}

(!:) :: Elt a => Array a -> Int -> a
{-# INLINE (!:) #-}

extract :: Elt a => Array a -> Int -> Int -> Array a
{-# INLINE extract #-}

drop :: Elt a => Int -> Array a -> Array a
{-# INLINE drop #-}

permute :: Elt a => Array a -> Array Int -> Array a
{-# INLINE permute #-}

bpermute :: Elt a => Array a -> Array Int -> Array a
{-# INLINE bpermute #-}

update :: Elt a => Array a -> Array (Int :*: a) -> Array a
{-# INLINE update #-}

(+:+) :: Elt a => Array a -> Array a -> Array a
{-# INLINE (+:+) #-}


pack :: Elt a => Array a -> Array Bool -> Array a
{-# INLINE pack #-}

combine :: Elt a => Array Bool -> Array a -> Array a -> Array a
{-# INLINE combine #-}

map :: (Elt a, Elt b) => (a -> b) -> Array a -> Array b
{-# INLINE map #-}

filter :: Elt a => (a -> Bool) -> Array a -> Array a
{-# INLINE filter #-}

zip :: (Elt a, Elt b) => Array a -> Array b -> Array (a :*: b)
{-# INLINE zip #-}

unzip :: (Elt a, Elt b) => Array (a :*: b) -> Array a :*: Array b
{-# INLINE unzip #-}

fsts  :: (Elt a, Elt b) => Array (a :*: b) -> Array a
{-# INLINE fsts #-}

zip3 :: (Elt a, Elt b, Elt c) => Array a -> Array b -> Array c
                           -> Array (a :*: b :*: c)
{-# INLINE zip3 #-}

unzip3 :: (Elt a, Elt b, Elt c)
       => Array (a :*: b :*: c) -> Array a :*: Array b :*: Array c
{-# INLINE unzip3 #-}

zipWith :: (Elt a, Elt b, Elt c)
        => (a -> b -> c) -> Array a -> Array b -> Array c
{-# INLINE zipWith #-}

zipWith3 :: (Elt a, Elt b, Elt c, Elt d)
          => (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
{-# INLINE zipWith3 #-}


fold :: Elt a => (a -> a -> a) -> a -> Array a -> a
{-# INLINE fold #-}

fold1 :: Elt a => (a -> a -> a) -> Array a -> a
{-# INLINE fold1 #-}

and :: Array Bool -> Bool
{-# INLINE and #-}

sum :: (Num a, Elt a) => Array a -> a
{-# INLINE sum #-}

scan :: Elt a => (a -> a -> a) -> a -> Array a -> Array a
{-# INLINE scan #-}


indexed :: Elt a => Array a -> Array (Int :*: a)
{-# INLINE indexed #-}

enumFromTo :: Int -> Int -> Array Int
{-# INLINE enumFromTo #-}

enumFromThenTo :: Int -> Int -> Int -> Array Int
{-# INLINE enumFromThenTo #-}

enumFromToEach :: Int -> Array (Int :*: Int) -> Array Int
{-# INLINE enumFromToEach #-}


concat :: Elt a => SArray a -> Array a
{-# INLINE concat #-}

(>:) :: Elt a => Segd -> Array a -> SArray a
{-# INLINE (>:) #-}

(^+:+^) :: Elt a => SArray a -> SArray a -> SArray a
{-# INLINE (^+:+^) #-}


length_s :: Elt a => SArray a -> Int
{-# INLINE length_s #-}

lengths_s :: Elt a => SArray a -> Array Int
{-# INLINE lengths_s #-}

replicate_s :: Elt a => Segd -> Array a -> SArray a
{-# INLINE replicate_s #-}

indices_s :: Elt a => SArray a -> Array Int
{-# INLINE indices_s #-}


fst_s :: (Elt a, Elt b) => SArray (a :*: b) -> SArray a
{-# INLINE fst_s #-}

snd_s :: (Elt a, Elt b) => SArray (a :*: b) -> SArray b
{-# INLINE snd_s #-}

zip_s :: (Elt a, Elt b) => SArray a -> SArray b -> SArray (a :*: b)
{-# INLINE zip_s #-}


bpermute_s' :: Elt a => Array a -> SArray Int -> SArray a
{-# INLINE bpermute_s' #-}


map_s:: (Elt a, Elt b) => (a -> b) -> SArray a -> SArray b
{-# INLINE map_s #-}

filter_s :: Elt a => (a -> Bool) -> SArray a -> SArray a
{-# INLINE filter_s #-}

pack_c :: Elt a => Array Bool -> SArray a -> SArray a
{-# INLINE pack_c #-}

combine_c :: Elt a => Array Bool -> SArray a -> SArray a -> SArray a
{-# INLINE combine_c #-}

zipWith_s :: (Elt a, Elt b, Elt c) 
          => (a -> b -> c) -> SArray a -> SArray b -> SArray c
{-# INLINE zipWith_s #-}


fold_s :: Elt a => (a -> a -> a) -> a -> SArray a -> Array a
{-# INLINE fold_s #-}

fold1_s :: Elt a => (a -> a -> a) -> SArray a -> Array a
{-# INLINE fold1_s #-}

sum_s :: (Num a, Elt a) => SArray a -> Array a
{-# INLINE sum_s #-}


enumFromThenTo_s :: Array Int -> Array Int -> Array Int -> SArray Int
{-# INLINE enumFromThenTo_s #-}


indexed_s :: Elt a => SArray a -> SArray (Int :*: a)
{-# INLINE indexed_s #-}


lengthsSegd :: Segd -> Array Int
{-# INLINE lengthsSegd #-}

lengthsToSegd :: Array Int -> Segd
{-# INLINE lengthsToSegd #-}

toSegd :: Array (Int :*: Int) -> Segd
{-# INLINE toSegd #-}

fromSegd :: Segd -> Array (Int :*: Int)
{-# INLINE fromSegd #-}


randoms :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
        => Int -> g -> Array a
{-# INLINE randoms #-}

randomRs :: (Elt a, System.Random.Random a, System.Random.RandomGen g)
          => Int -> (a,a) -> g -> Array a
{-# INLINE randomRs #-}


instance IOElt Int
instance IOElt Double
instance (IOElt a, IOElt b) => IOElt (a :*: b)

hPut :: IOElt a => Handle -> Array a -> IO ()
{-# INLINE hPut #-}

hGet :: IOElt a => Handle -> IO (Array a)
{-# INLINE hGet #-}

toList :: Elt a => Array a -> [a]
{-# INLINE toList #-}

fromList :: Elt a => [a] -> Array a
{-# INLINE fromList #-}

toList_s :: Elt a => SArray a -> [[a]]
{-# INLINE toList_s #-}

fromList_s :: Elt a => [[a]] -> SArray a
{-# INLINE fromList_s #-}

