module Data.Array.Parallel.Lifted.Closure (
  (:->)(..), PArray(..),
  mkClosure, mkClosureP, ($:), ($:^),
  closure, liftedClosure, liftedApply,
  dPA_Clo, dPR_Clo,

  closure1, closure2, closure3
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Instances  (dPA_Unit, dPA_2, dPA_3)
import Data.Array.Parallel.Lifted.Repr

import GHC.Exts (Int#)

infixr 0 :->
infixl 0 $:, $:^

-- |The type of closures
--
data a :-> b = forall e. Clo (PA e)
                             !(e -> a -> b)
                             !(Int# -> PData e -> PData a -> PData b)
                             e

lifted :: (PArray e -> PArray a -> PArray b)
       -> Int# -> PData e -> PData a -> PData b
{-# INLINE lifted #-}
lifted f n# es as = case f (PArray n# es) (PArray n# as) of PArray _ bs -> bs

-- |Closure construction
--
mkClosure :: forall a b e. 
             PA e -> (e -> a -> b)
                  -> (PArray e -> PArray a -> PArray b)
                  -> e -> (a :-> b)
{-# INLINE CONLIKE mkClosure #-}
mkClosure pa fv fl e = Clo pa fv (lifted fl) e

closure :: forall a b e.
           PA e -> (e -> a -> b)
                -> (Int# -> PData e -> PData a -> PData b)
                -> e
                -> (a :-> b)
{-# INLINE closure #-}
closure pa fv fl e = Clo pa fv fl e

-- |Closure application
--
($:) :: forall a b. (a :-> b) -> a -> b
{-# INLINE ($:) #-}
Clo _ f _ e $: a = f e a

{-# RULES

"mkClosure/($:)" forall pa fv fl e x.
  mkClosure pa fv fl e $: x = fv e x

 #-}

-- |Arrays of closures (aka array closures)
--
data instance PData (a :-> b)
  = forall e. AClo (PA e)
                   !(e -> a -> b)
                   !(Int# -> PData e -> PData a -> PData b)
                    (PData e)

-- |Lifted closure construction
--
mkClosureP :: forall a b e.
              PA e -> (e -> a -> b)
                   -> (PArray e -> PArray a -> PArray b)
                   -> PArray e -> PArray (a :-> b)
{-# INLINE mkClosureP #-}
mkClosureP pa fv fl (PArray n# es) = PArray n# (AClo pa fv (lifted fl) es)

liftedClosure :: forall a b e.
                 PA e -> (e -> a -> b)
                      -> (Int# -> PData e -> PData a -> PData b)
                      -> PData e
                      -> PData (a :-> b)
{-# INLINE liftedClosure #-}
liftedClosure pa fv fl es = AClo pa fv fl es

-- |Lifted closure application
--
($:^) :: forall a b. PArray (a :-> b) -> PArray a -> PArray b
{-# INLINE ($:^) #-}
PArray n# (AClo _ _ f es) $:^ PArray _ as = PArray n# (f n# es as)

liftedApply :: forall a b. Int# -> PData (a :-> b) -> PData a -> PData b
{-# INLINE liftedApply #-}
liftedApply n# (AClo _ _ f es) as = f n# es as

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
            emptyPR      = emptyPR_Clo
          , replicatePR  = replicatePR_Clo
          , replicatelPR = replicatelPR_Clo
          , indexPR      = indexPR_Clo
          , bpermutePR   = bpermutePR_Clo
          , packPR       = packPR_Clo
          }

{-# INLINE emptyPR_Clo #-}
emptyPR_Clo = AClo dPA_Unit (\e  a  -> error "empty array closure")
                            (\es as -> error "empty array closure")
                            (emptyPD dPA_Unit)

{-# INLINE replicatePR_Clo #-}
replicatePR_Clo n# (Clo pa f f' e) = AClo pa f f' (replicatePD pa n# e)

{-# INLINE replicatelPR_Clo #-}
replicatelPR_Clo segd (AClo pa f f' es)
  = AClo pa f f' (replicatelPD pa segd es)

{-# INLINE indexPR_Clo #-}
indexPR_Clo (AClo pa f f' es) i# = Clo pa f f' (indexPD pa es i#)

{-# INLINE bpermutePR_Clo #-}
bpermutePR_Clo (AClo pa f f' es) n# is = AClo pa f f' (bpermutePD pa es n# is)

{-# INLINE packPR_Clo #-}
packPR_Clo (AClo pa f f' es) n# sel = AClo pa f f' (packPD pa es n# sel)

-- Closure construction

closure1 :: (a -> b) -> (PArray a -> PArray b) -> (a :-> b)
{-# INLINE closure1 #-}
closure1 fv fl = mkClosure dPA_Unit (\_ -> fv) (\_ -> fl) ()

closure2 :: PA a
         -> (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)
{-# INLINE closure2 #-}
closure2 pa fv fl = mkClosure dPA_Unit fv_1 fl_1 ()
  where
    fv_1 _ x  = mkClosure  pa fv fl x
    fl_1 _ xs = mkClosureP pa fv fl xs

closure3 :: PA a -> PA b
         -> (a -> b -> c -> d)
         -> (PArray a -> PArray b -> PArray c -> PArray d)
         -> (a :-> b :-> c :-> d)
{-# INLINE closure3 #-}
closure3 pa pb fv fl = mkClosure dPA_Unit fv_1 fl_1 ()
  where
    fv_1 _  x  = mkClosure  pa fv_2 fl_2 x
    fl_1 _  xs = mkClosureP pa fv_2 fl_2 xs

    fv_2 x  y  = mkClosure  (dPA_2 pa pb) fv_3 fl_3 (x,y)
    fl_2 xs ys = mkClosureP (dPA_2 pa pb) fv_3 fl_3 (zipPA# xs ys)

    fv_3 (x,y) z = fv x y z
    fl_3 ps zs = case unzipPA# ps of (xs,ys) -> fl xs ys zs

