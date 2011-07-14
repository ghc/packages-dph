{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators,
        NoMonomorphismRestriction #-}

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray

arr10   = fromListPA [1..10 :: Int]
arr5    = fromListPA [1..5  :: Int]

-- length examples
ex_length
 = lengthPP $: arr10
 
ex_length_l
 = mapPP $: lengthPP $: fromListPA [arr5, arr10, fromListPA [1..100]]


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
 
 




-- smvm -----------------------------------------------------------------------
matrix :: PArray (PArray (Int, Double))
matrix  = fromListPA    [ fromListPA [(0, 1), (10, 5), (100, 20)]
                        , fromListPA [(0, 2), (20, 6), (90,  30)] ]

vector  :: PArray Double
vector  = fromListPA    [0..99 :: Double]


smvm :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
smvm m v
        = v_smvm $: m $: v


lap = liftedApply

-- smvm = (\m v. mapP (\r. sumP (mapP (\z. case z of (i, x) -> x * (v !: i)))) m)
v_smvm :: PArray (PArray (Int, Double)) :-> PArray Double :-> PArray Double
v_smvm          = Clo (\_ m -> Clo vsmvm undefined m) undefined ()


-- mapP (\r. sumP (mapP (\z. case z of (i, x) -> x * (v !: i)) r)) m  
vsmvm :: PArray (PArray (Int, Double)) -> PArray Double -> PArray Double
vsmvm m v      = mapPP $: (Clo undefined lsmvm2 v) $: m


-- (\r. sumP (mapP (\z. case z of (i, x) -> x * (v !: i)) r))
lsmvm2  :: forall m1 m2
        .  (PJ m1 (PArray Double), PJ m2 (PArray (Int, Double)))
        => Int -> PData m1 (PArray Double) -> PData m2 (PArray (Int, Double)) -> PData Sized Double
lsmvm2 c vs rs 
        = lap c (repeatPE sumPP_double) 
                (lap c  (lap c  (repeatPE mapPP) 
                                (AClo undefined lsmvm3 vs))
                        rs)

-- \z. case z of (i, x) -> x * (v !: i)
lsmvm3  :: forall m1 m2
        .  (PJ m1 (PArray Double), PJ m2 (Int, Double))
        => Int -> PData m1 (PArray Double) -> PData m2 (Int, Double) -> PData Sized Double
lsmvm3 c vs zs
 = case restrictPJ c zs of
    PTuple2 is xs
     -> lap c    (lap c (repeatPE multPP_double) xs)
                 (lap c (lap c (repeatPE indexPP) vs) is)

