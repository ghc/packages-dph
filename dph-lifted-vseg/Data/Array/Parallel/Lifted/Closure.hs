{-# LANGUAGE
        CPP,
        TypeOperators, ScopedTypeVariables, ExistentialQuantification,
        TypeFamilies, Rank2Types, MultiParamTypeClasses, 
        StandaloneDeriving, FlexibleContexts #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Closure (
  -- * Closures.
  (:->)(..),
  ($:),
  closure1, closure2, closure3,
  
  -- * Array Closures.
  PData(..),
  liftedApply
) where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Unit
import Data.Array.Parallel.PArray.PData.Tuple


-- Closures -------------------------------------------------------------------

-- | Define the fixity of the closure type constructor.
infixr 0 :->

-- | The type of closures.
--   This bundles up:
---   1) the 'vectorised' version of the function that takes an explicit environment
--    2) the 'lifted' version, that works on arrays.
--       The first parameter of the lifted version is the 'lifting context'
--       that gives the length of the arrays being operated on.
--    3) the environment of the closure.
-- 
--   The vectoriser closure-converts the source program so that all functions
--   types are expressed in this form.
data (a :-> b)
        = forall env. PR env
        => Clo  (env -> a -> b)
                (Int -> PData env -> PData a -> PData b)
                env


-- | Closure application.
($:) :: (a :-> b) -> a -> b
{-# INLINE_CLOSURE ($:) #-}
($:) (Clo fv _fl env) x  = fv env x
   

-- Array Closures -------------------------------------------------------------
-- | Arrays of closures (aka array closures)
--   We need to represent arrays of closures when vectorising partial applications.
--
--   For example, consider:
--     @mapP (+) xs   ::  [: Int -> Int :]@
--
--   Representing this an array of thunks doesn't work because we can't evaluate
--   in a data parallel manner. Instead, we want *one* function applied to many
--   array elements.
-- 
--   Instead, such an array of closures is represented as the vectorised 
--   and lifted versions of (+), along with an environment array xs that
--   contains the partially applied arguments.
--
--     @mapP (+) xs  ==>  AClo plus_v plus_l xs@
--
data instance PData (a :-> b)
        = forall env. PR env
        => AClo (env -> a -> b)
                (Int -> PData env -> PData a -> PData b)
                (PData env)


-- | Lifted closure application.
liftedApply 
        :: Int -> PData (a :-> b) -> PData a -> PData b

{-# INLINE_CLOSURE liftedApply #-}
liftedApply n (AClo _ fl envs) as
        = fl n envs as


-- Closure Construction -------------------------------------------------------
-- These functions are used for building closure representations of primitive
-- functions. Their used in D.A.P.Lifted.Combinators where we define the 
-- array combinators that vectorised code uses.

-- | Construct an arity-1 closure.
--   from unlifted and lifted versions of a primitive function.
closure1 
        :: (a -> b)
        -> (Int -> PData a -> PData b)
        -> (a :-> b)

{-# INLINE_CLOSURE closure1 #-}
closure1 fv fl  
        = Clo   (\_env -> fv)
                (\n _env -> fl n)
                ()


-- | Construct an arity-2 closure
--   from lifted and unlifted versions of a primitive function.
closure2 
        :: forall a b c. PR a
        => (a -> b -> c)
        -> (Int -> PData a -> PData b -> PData c)
        -> (a :-> b :-> c)

{-# INLINE_CLOSURE closure2 #-}
closure2 fv fl
 = let  fv_1 _ xa   = Clo fv fl xa
        fl_1 _ _ xs = AClo fv fl xs
        
   in   Clo fv_1 fl_1 ()


-- | Construct an arity-3 closure
--   from lifted and unlifted versions of a primitive function.
closure3 
        :: forall a b c d. (PR a, PR b)
        => (a -> b -> c -> d)
        -> (Int -> PData a -> PData b -> PData c -> PData d)
        -> (a :-> b :-> c :-> d)
        
{-# INLINE_CLOSURE closure3 #-}
closure3 fv fl
 = let  fv_1   _ xa = Clo   fv_2 fl_2 xa
        fl_1 _ _ xs = AClo  fv_2 fl_2 xs

        -----
        fv_2 xa yb   = Clo  fv_3 fl_3 (xa, yb)
        fl_2 _ xs ys = AClo fv_3 fl_3 (PTuple2 xs ys)

        -----
        fv_3 (xa, yb) zc           = fv xa yb zc
        fl_3 n (PTuple2 xs ys) zs  = fl n xs ys zs

   in   Clo fv_1 fl_1 ()

