module Data.Array.Parallel.Lifted.Closure (
  (:->)(..), PArray(..),
  mkClosure, mkClosureP, ($:), ($:^),
  dPA_Clo, dPR_Clo,

  closure1, closure2
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Instances  (dPA_Unit)

infixr 0 :->
infixl 0 $:, $:^

-- |The type of closures
--
data a :-> b = forall e. Clo (PA e)
                             !(e -> a -> b)
                             !(PArray e -> PArray a -> PArray b)
                             e

-- |Closure construction
--
mkClosure :: forall a b e. 
             PA e -> (e -> a -> b)
                  -> (PArray e -> PArray a -> PArray b)
                  -> e -> (a :-> b)
{-# INLINE mkClosure #-}
mkClosure = Clo

-- |Closure application
--
($:) :: forall a b. (a :-> b) -> a -> b
{-# INLINE ($:) #-}
Clo _ f _ e $: a = f e a

-- |Arrays of closures (aka array closures)
--
data instance PArray (a :-> b)
  = forall e. AClo (PA e)
                   !(e -> a -> b)
                   !(PArray e -> PArray a -> PArray b)
                   !(PArray e)

-- |Lifted closure construction
--
mkClosureP :: forall a b e.
              PA e -> (e -> a -> b)
                   -> (PArray e -> PArray a -> PArray b)
                   -> PArray e -> PArray (a :-> b)
{-# INLINE mkClosureP #-}
mkClosureP = AClo

-- |Lifted closure application
--
($:^) :: forall a b. PArray (a :-> b) -> PArray a -> PArray b
{-# INLINE ($:^) #-}
AClo _ _ f es $:^ as = f es as

type instance PRepr (a :-> b) = a :-> b

dPA_Clo :: PA a -> PA b -> PA (a :-> b)
{-# INLINE dPA_Clo #-}
dPA_Clo _ _ = PA {
                toPRepr      = id
              , fromPRepr    = id
              , toArrPRepr   = id
              , fromArrPRepr = id
              , dictPRepr    = dPR_Clo
              }

dPR_Clo :: PR (a :-> b)
{-# INLINE dPR_Clo #-}
dPR_Clo = PR {
            lengthPR    = lengthPR_Clo
          , emptyPR     = emptyPR_Clo
          , replicatePR = replicatePR_Clo
          , packPR      = packPR_Clo
          }

{-# INLINE lengthPR_Clo #-}
lengthPR_Clo (AClo pa f f' es) = lengthPA pa es

{-# INLINE emptyPR_Clo #-}
emptyPR_Clo = AClo dPA_Unit (\e  a  -> error "empty array closure")
                            (\es as -> error "empty array closure")
                            (emptyPA dPA_Unit)

{-# INLINE replicatePR_Clo #-}
replicatePR_Clo n# (Clo pa f f' e) = AClo pa f f' (replicatePA pa n# e)

{-# INLINE packPR_Clo #-}
packPR_Clo (AClo pa f f' es) n# sel# = AClo pa f f' (packPA pa es n# sel#)

closure1 :: (a -> b) -> (PArray a -> PArray b) -> (a :-> b)
{-# INLINE closure1 #-}
closure1 fv fl = Clo dPA_Unit (\_ -> fv) (\_ -> fl) ()

closure2 :: PA a
         -> (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)
{-# INLINE closure2 #-}
closure2 pa fv fl = Clo dPA_Unit fv_1 fl_1 ()
  where
    fv_1 _ x  = Clo  pa fv fl x
    fl_1 _ xs = AClo pa fv fl xs

