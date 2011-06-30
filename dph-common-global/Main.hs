{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}

import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.PArray


-- plus example
--   The constant 5 is replicated by the implementation of plusPA_l.
ex_plus	 
 = mapPA $: (plusPA $: 5) 
	 $: (fromListPA [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 :: Int])


-- index example
--   The source array for the indexing operation is not replicated
--   because the lifted indexing operator is special-cased to use
--   the same array.
ex_index 
 = mapPA $: (indexPA $: (fromListPA [1, 2, 3, 4, 5, 6, 7 :: Int])) 
	 $: (fromListPA [1, 2, 3 :: Int])