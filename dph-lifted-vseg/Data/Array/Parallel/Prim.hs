
-- | This is the API used by the vectoriser.
--   The vectoriser wants a slightly different interface to the one used 
--   natively by the library. This module performs the impedance matching.
module Data.Array.Parallel.Prim 
        ( emptyPD#
        , replicatePD#
        , packByTagPD#
        , combine2PD#

        -- * Closures
        , closure,              ($:)
        , liftedClosure,        liftedApply
        , closure1
        , closure2
        , closure3
        
        -- * Selectors
        , Sel2
        , replicateSel2#
        , pickSel2#
        , tagsSel2
        , elementsSel2_0#
        , elementsSel2_1#)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PRepr.Base
import GHC.Exts
import Data.Array.Parallel.Base
import qualified Data.Array.Parallel.Unlifted           as U

import qualified Data.Array.Parallel.Lifted.Closure     as C
import Data.Array.Parallel.Lifted.Closure     ((:->)(..))


-- Constructors ---------------------------------------------------------------
-- The vectoriser wants versions of these functions that take unboxed integers
-- for some of the arguments.
{-# INLINE emptyPD# #-}
emptyPD# :: PA a => PData a
emptyPD# = emptyPD

{-# INLINE replicatePD# #-}
replicatePD# :: PA a => Int# -> a -> PData a
replicatePD# r x 
        = replicatePD (I# r) x


-- Closures -------------------------------------------------------------------
-- The vectoriser wants versions of these functions that take unboxed
-- integers for the first argument of the lifted function.

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



-- Packing and Combining ------------------------------------------------------
-- The vectoriser wants versions of these that take unboxed integers
-- for some arguments.
{-# INLINE packByTagPD# #-}
packByTagPD# :: PA a => PData a -> Int# -> U.Array Tag -> Int# -> PData a
packByTagPD# arr _ tags t
        = packByTagPD arr tags (I# t)


{-# INLINE combine2PD# #-}
combine2PD# :: PA a => Int# -> U.Sel2 -> PData a -> PData a -> PData a
combine2PD# _ sel pdata1 pdata2
        = combine2PD sel pdata1 pdata2


-- Selector functions ---------------------------------------------------------
-- The vectoriser wants versions of these that take unboxed integers
-- for some arguments.
type Sel2       = U.Sel2


{-# INLINE replicateSel2# #-}
replicateSel2# :: Int# -> Int# -> Sel2
replicateSel2# n# tag#
  = U.mkSel2
         (U.replicate n (intToTag tag))
         (U.enumFromStepLen 0 1 n)
         (if tag == 0 then n else 0)
         (if tag == 0 then 0 else n)
         (U.mkSelRep2 (U.replicate n (intToTag tag)))
  where
    n   = I# n#
    tag = I# tag#


{-# INLINE pickSel2# #-}
pickSel2# :: Sel2 -> Int# -> U.Array Bool
pickSel2# sel tag#
        = U.pick (U.tagsSel2 sel) (intToTag (I# tag#))


{-# INLINE tagsSel2 #-}
tagsSel2 :: Sel2 -> U.Array Tag
tagsSel2 = U.tagsSel2


{-# INLINE elementsSel2_0# #-}
elementsSel2_0# :: Sel2 -> Int#
elementsSel2_0# sel
        = case U.elementsSel2_0 sel of { I# n# -> n# }


{-# INLINE elementsSel2_1# #-}
elementsSel2_1# :: Sel2 -> Int#
elementsSel2_1# sel
        = case U.elementsSel2_1 sel of { I# n# -> n# }
