instance Elt Int
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

bpermute :: Elt a => Array a -> Array Int -> Array a
{-# INLINE bpermute #-}

(+:+) :: Elt a => Array a -> Array a -> Array a
{-# INLINE (+:+) #-}


pack :: Elt a => Array a -> Array Bool -> Array a
{-# INLINE pack #-}

combine :: Elt a => Array Bool -> Array a -> Array a -> Array a
{-# INLINE combine #-}

map :: (Elt a, Elt b) => (a -> b) -> Array a -> Array b
{-# INLINE map #-}

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

sum :: (Num a, Elt a) => Array a -> a
{-# INLINE sum #-}

scan :: Elt a => (a -> a -> a) -> a -> Array a -> Array a
{-# INLINE scan #-}


indexed :: Elt a => Array a -> Array (Int :*: a)
{-# INLINE indexed #-}

enumFromTo :: Int -> Int -> Array Int
{-# INLINE enumFromTo #-}

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

indices_s :: Elt a => SArray a -> Array Int
{-# INLINE indices_s #-}


fold_s :: Elt a => (a -> a -> a) -> a -> SArray a -> Array a
{-# INLINE fold_s #-}

fold1_s :: Elt a => (a -> a -> a) -> SArray a -> Array a
{-# INLINE fold1_s #-}

sum_s :: (Num a, Elt a) => SArray a -> Array a
{-# INLINE sum_s #-}


indexed_s :: Elt a => SArray a -> SArray (Int :*: a)
{-# INLINE indexed_s #-}


toSegd :: Array (Int :*: Int) -> Segd
{-# INLINE toSegd #-}

