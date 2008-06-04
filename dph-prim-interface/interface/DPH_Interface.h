infixl 9 !:
infixr 5 +:+
infixr 5 ^+:+^
infixr 9 >:

length :: UA a => UArr a -> Int
empty :: UA a => UArr a
replicate :: UA a => Int -> a -> UArr a
replicateEach :: UA a => Int -> UArr Int -> UArr a -> UArr a
repeat :: UA a => Int -> UArr a -> UArr a
(!:) :: UA a => UArr a -> Int -> a
bpermute :: UA a => UArr a -> UArr Int -> UArr a
(+:+) :: UA a => UArr a -> UArr a -> UArr a

pack :: UA a => UArr a -> UArr Bool -> UArr a
combine :: UA a => UArr Bool -> UArr a -> UArr a -> UArr a

map :: (UA a, UA b) => (a -> b) -> UArr a -> UArr b
zip :: (UA a, UA b) => UArr a -> UArr b -> UArr (a :*: b)
unzip :: (UA a, UA b) => UArr (a :*: b) -> UArr a :*: UArr b
fsts  :: (UA a, UA b) => UArr (a :*: b) -> UArr a
zip3 :: (UA a, UA b, UA c) => UArr a -> UArr b -> UArr c
                           -> UArr (a :*: b :*: c)
unzip3 :: (UA a, UA b, UA c)
       => UArr (a :*: b :*: c) -> UArr a :*: UArr b :*: UArr c
zipWith :: (UA a, UA b, UA c) => (a -> b -> c) -> UArr a -> UArr b -> UArr c
zipWith3 :: (UA a, UA b, UA c, UA d)
          => (a -> b -> c -> d) -> UArr a -> UArr b -> UArr c -> UArr d

fold :: UA a => (a -> a -> a) -> a -> UArr a -> a
fold1 :: UA a => (a -> a -> a) -> UArr a -> a
sum :: (Num a, UA a) => UArr a -> a
scan :: UA a => (a -> a -> a) -> a -> UArr a -> UArr a

indexed :: UA a => UArr a -> UArr (Int :*: a)
enumFromTo :: Int -> Int -> UArr Int
enumFromToEach :: Int -> UArr (Int :*: Int) -> UArr Int

concat :: UA a => SUArr a -> UArr a
(>:) :: UA a => USegd -> UArr a -> SUArr a
(^+:+^) :: UA a => SUArr a -> SUArr a -> SUArr a

length_s :: UA a => SUArr a -> Int
lengths_s :: UA a => SUArr a -> UArr Int
indices_s :: UA a => SUArr a -> UArr Int

fold_s :: UA a => (a -> a -> a) -> a -> SUArr a -> UArr a
fold1_s :: UA a => (a -> a -> a) -> SUArr a -> UArr a
sum_s :: (Num a, UA a) => SUArr a -> UArr a

indexed_s :: UA a => SUArr a -> SUArr (Int :*: a)

toUSegd :: UArr (Int :*: Int) -> USegd

