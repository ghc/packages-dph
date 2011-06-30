{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}

module Data.Array.Parallel.Lifted.Combinators where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray

import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)

-- fromList -------------------------------------------------------------------
fromListPA :: PR a => [a] -> PArray a
fromListPA xs
	= PArray (length xs) (fromListPS xs)


-- length ---------------------------------------------------------------------
lengthPA   :: PArray a :-> Int
lengthPA = closure1 lengthPA_v lengthPA_l

-----
lengthPA_v :: PArray a -> Int
lengthPA_v (PArray n _)	= n

-----
lengthPA_l :: PJ m (PArray a)
	   => Int -> PData m (PArray a) -> PData Sized Int

lengthPA_l c da
 = let	PNestedS segd _	= restrictPJ c da
   in	undefined -- TODO


-- map ------------------------------------------------------------------------
mapPA   :: (PR a, PR b) 
        => (a :-> b) :-> PArray a :-> PArray b
mapPA 	= closure2 mapPA_v mapPA_l

-----
mapPA_v :: (PR a, PR b)
	=> (a :-> b) -> PArray a -> PArray b

mapPA_v (Clo fv fl env) (PArray n as) 
	= PArray n (fl n (repeatPE env) as)

-----
mapPA_l :: (PJ m1 (a :-> b), PJ m2 (PArray a))
	=> Int 	-> PData m1 (a :-> b) 
		-> PData m2 (PArray a) -> PData Sized (PArray b)

mapPA_l n clo arr2
 = let	PNestedS segd2 d2	= restrictPJ n arr2
   in	undefined -- TODO


-- index ----------------------------------------------------------------------
indexPA :: PR a => PArray a :-> Int :-> a
indexPA	= closure2 indexPA_v indexPA_l

indexPA_v :: PR a => PArray a -> Int -> a
indexPA_v (PArray _ d1) ix
	= indexPJ d1 ix

indexPA_l :: (PJ m1 (PArray a), PJ m2 Int)
	=> Int -> PData m1 (PArray a) -> PData m2 Int -> PData Sized a

indexPA_l n src ixs
 = case indexlPJ n src (restrictPJ n ixs) of
	PArray _ d	-> d


-- plus -----------------------------------------------------------------------
plusPA	:: Int :-> Int :-> Int
plusPA	 = closure2 plusPA_v plusPA_l

plusPA_v  :: Int -> Int -> Int
plusPA_v = (+)

plusPA_l  :: (PJ m1 Int, PJ m2 Int)
        => Int -> PData m1 Int -> PData m2 Int -> PData Sized Int
plusPA_l n d1 d2
 = let PIntS vec1 = restrictPJ n d1
       PIntS vec2 = restrictPJ n d2
   in  PIntS (V.zipWith (+) vec1 vec2)

