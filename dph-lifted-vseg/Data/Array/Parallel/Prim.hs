
-- | This is the API used by the vectoriser.
module Data.Array.Parallel.Prim 
        ( emptyPD#
        , replicatePD#
        , packByTagPD#

        -- * Closures
        , closure,              ($:)
        , liftedClosure,        liftedApply
        , closure1
        , closure2
        , closure3)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PRepr.Base
import GHC.Exts
import Data.Array.Parallel.Base                         (Tag)
import qualified Data.Array.Parallel.Unlifted           as U

import qualified Data.Array.Parallel.Lifted.Closure     as C
import Data.Array.Parallel.Lifted.Closure     ((:->)(..))



-- Array Operators ------------------------------------------------------------
-- taking unboxed integers for the arguments.
{-# INLINE emptyPD# #-}
emptyPD# :: PA a => PData a
emptyPD# = emptyPD

{-# INLINE replicatePD# #-}
replicatePD# :: PA a => Int# -> a -> PData a
replicatePD# r x 
        = replicatePD (I# r) x

{-# INLINE packByTagPD# #-}
packByTagPD# :: PA a => PData a -> Int# -> U.Array Tag -> Int# -> PData a
packByTagPD# arr _ tags t
        = packByTagPD arr tags (I# t)


-- Closures -------------------------------------------------------------------
-- | Construct a closure.
{-# INLINE closure #-}
closure :: forall a b e
        .  PA e
        => (e -> a -> b)
        -> (Int# -> PData e -> PData a -> PData b)
        -> e
        -> (a :-> b)

closure fv fl e 
 = Clo fv 
         (\(I# c) v x -> fl c v x)
         e


-- | Apply a closure.
{-# INLINE ($:) #-}
($:)    = (C.$:)


-- | Construct a lifted closure.
{-# INLINE liftedClosure #-}
liftedClosure
        :: forall a b e
        .  PA e
        => (e -> a -> b)
        -> (Int# -> PData e -> PData a -> PData b)
        -> PData e
        -> PData (a :-> b)

liftedClosure fv fl es
 = C.AClo fv 
        (\(I# c) v x -> fl c v x)
        es
        
-- | Apply a lifted closure.
{-# INLINE liftedApply #-}
liftedApply :: Int# -> PData (a :-> b) -> PData a -> PData b
liftedApply n# arr xs
        = C.liftedApply (I# n#) arr xs


{-# INLINE closure1 #-}
closure1 :: forall a b
         .  (a -> b)
         -> (PArray a -> PArray b)
         -> (a :-> b)
closure1 fv fl
 = let  fl' :: Int -> PData a -> PData b
        fl' (I# c#) pdata 
         = case fl (PArray c# pdata) of
                 PArray _ pdata' -> pdata'
                        
   in   C.closure1 fv fl'


{-# INLINE closure2 #-}
closure2 :: forall a b c. PA a
         => (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)
closure2 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c
        fl' (I# c#) pdata1 pdata2
         = case fl (PArray c# pdata1) (PArray c# pdata2) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure2 fv fl'


{-# INLINE closure3 #-}
closure3 :: forall a b c d.  (PA a, PA b)
         => (a -> b -> c -> d)
         -> (PArray a -> PArray b -> PArray c -> PArray d)
         -> (a :-> b :-> c :-> d)
closure3 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d
        fl' (I# c#) pdata1 pdata2 pdata3
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure3 fv fl'
