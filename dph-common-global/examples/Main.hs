{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators,
        NoMonomorphismRestriction #-}

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray

main 
 = do   putStrLn $ (show $ lengthPA arr10)
        putStr "foo"

arr10           = fromListPA [1..10 :: Int]
arr5            = fromListPA [1..5  :: Int]
arrN            = fromListPA [arr5, arr10, fromListPA [1..100]]

arrError        :: PArray (PArray Int)
arrError        = error "denied!"

lap = liftedApply

-- length examples
ex_length
 = lengthPP $: arr10
 
ex_length_l
 = mapPP $: lengthPP $: arrN


-- plus examples
--   The constant 5 is replicated by the implementation of plusPA_l.
ex_plus_l	 
 = mapPP $: (plusPP_int $: 5) $: arr10


-- index examples
--   The source array for the indexing operation is not replicated
--   because the lifted indexing operator is special-cased to use
--   the same array.
ex_index_l
 = mapPP $: (indexPP $: arr10) $: arr5
 

ex_plus2
 = lap 10 (repeatPE (plusPP_int $: 2)) (repeatPE 3)
 
ex_plus3
 = lap 10 (lap 10 (repeatPE (plus3PP_int $: 2)) (repeatPE 3)) (repeatPE 4)


ignorePP :: (PR a, PR b) => a :-> b :-> b
ignorePP = closure2 ignore ignore_l

ignore :: a -> b -> b
ignore     _ b  = b

ignore_l
        :: forall a b m1 m2
        .  (PJ m1 a, PJ m2 b)
        => Int -> PData m1 a -> PData m2 b -> PData Sized b

ignore_l n _ bs 
        = restrictPJ n bs

ex_ignore xs
        = mapPP $: (mapPP $: (ignorePP $: arrError)) $: xs



