{-# LANGUAGE
	TypeOperators, ScopedTypeVariables, ExistentialQuantification,
	TypeFamilies, Rank2Types, MultiParamTypeClasses, 
        StandaloneDeriving #-}

module Data.Array.Parallel.Lifted.Closure where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Unit


-- Closures -------------------------------------------------------------------
infixr 0 :->
data (a :-> b)
	= forall env. PR env
	=> Clo 	(env -> a -> b)
		(forall m1 m2
			.  (PJ m1 env, PJ m2 a)
			=> Int -> PData m1 env -> PData m2 a -> PData Sized b)
		env


-- | Closure application.
($:) :: (a :-> b) -> a -> b
($:) (Clo fv fl env) x	= fv env x


-- | Construct an arity-1 closure.
closure1 
	:: (a -> b)
	-> (forall m2. PJ m2 a => Int -> PData m2 a -> PData Sized b)
	-> (a :-> b)
closure1 fv fl	
	= Clo	(\_env -> fv)
		(\n _env -> fl n)
		()


-- | Construct an arity-2 closure.
closure2 
	:: forall a b c. PR a
	=> (a -> b -> c)
	-> (forall m1 m2
		.  (PJ m1 a, PJ m2 b)
		=> Int -> PData m1 a -> PData m2 b -> PData Sized c)
	-> (a :-> b :-> c)

closure2 fv fl
 = let	fv_1 	:: forall env. env -> a -> (b :-> c)
	fv_1 _ xa = Clo fv fl xa

	fl_1 	:: forall env m1 m2
		.  (PJ m1 env, PJ m2 a)
		=> Int -> PData m1 env -> PData m2 a -> PData Sized (b :-> c)

	fl_1 n _ xs = AClo fv fl (restrictPJ n xs)
	
   in	Clo fv_1 fl_1 ()


-- Array Closures -------------------------------------------------------------
data instance PData m (a :-> b)
	= forall env. (PJ m env, PR env)
	=> AClo	(env -> a -> b)
		(forall m1 m2
			.  (PJ m1 env, PJ m2 a)
			=> Int -> PData m1 env -> PData m2 a -> PData Sized b)
		(PData m env)


-- | Lifted closure application.
liftedApply 
	:: PJ m2 a
	=> Int -> PData m1 (a :-> b) -> PData m2 a -> PData Sized b

liftedApply n (AClo _ fl envs) as
	= fl n envs as

