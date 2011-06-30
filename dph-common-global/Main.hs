{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray

arr10   = fromListPA [1..10 :: Int]
arr5    = fromListPA [1..5  :: Int]

-- length examples
ex_length
 = lengthPP $: arr10
 
ex_length_l
 = mapPP $: lengthPP $: fromListPA [arr5, arr10]


-- plus examples
--   The constant 5 is replicated by the implementation of plusPA_l.
ex_plus_l	 
 = mapPP $: (plusPP $: 5) $: arr10


-- index examples
--   The source array for the indexing operation is not replicated
--   because the lifted indexing operator is special-cased to use
--   the same array.
ex_index_l
 = mapPP $: (indexPP $: arr10) $: arr5