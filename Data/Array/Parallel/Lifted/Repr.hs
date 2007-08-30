module Data.Array.Parallel.Lifted.Repr (
  PArray(..),
  Id(..),
  Sum2(..), Sum3(..), 

  dPR_Unit, dPR_Id,
  dPR_2, dPR_3,
  dPR_Sum2, dPR_Sum3
) where

import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Prim
import Data.Array.Parallel.Unlifted

import GHC.Exts  (Int#, Int(..))

data instance PArray () = PUnit Int# ()

dPR_Unit :: PR ()
{-# INLINE dPR_Unit #-}
dPR_Unit = PR {
             lengthPR    = lengthPR_Unit
           , emptyPR     = emptyPR_Unit
           , replicatePR = replicatePR_Unit
           }
         

{-# INLINE lengthPR_Unit #-}
lengthPR_Unit (PUnit n# _) = n#

{-# INLINE emptyPR_Unit #-}
emptyPR_Unit = PUnit 0# ()

{-# INLINE replicatePR_Unit #-}
replicatePR_Unit n# u = PUnit n# u


data Id a = Id a

data instance PArray (Id a) = PId Int# (PArray a)

dPR_Id :: PR a -> PR (Id a)
{-# INLINE dPR_Id #-}
dPR_Id pr = PR {
              lengthPR    = lengthPR_Id
            , emptyPR     = emptyPR_Id pr
            , replicatePR = replicatePR_Id pr
            }

{-# INLINE lengthPR_Id #-}
lengthPR_Id (PId n# _) = n#

{-# INLINE emptyPR_Id #-}
emptyPR_Id pr = PId 0# (emptyPR pr)

{-# INLINE replicatePR_Id #-}
replicatePR_Id pr n# x = PId n# (case x of Id y -> replicatePR pr n# y)


data instance PArray (a,b)
  = P_2 Int# (PArray a)
             (PArray b)

data instance PArray (a,b,c)
  = P_3 Int# (PArray a)
             (PArray b)
             (PArray c)

dPR_2 :: PR a -> PR b -> PR (a,b)
{-# INLINE dPR_2 #-}
dPR_2 pra prb
  = PR {
      lengthPR    = lengthPR_2
    , emptyPR     = emptyPR_2 pra prb
    , replicatePR = replicatePR_2 pra prb
    }

{-# INLINE lengthPR_2 #-}
lengthPR_2 (P_2 n# _ _) = n#

{-# INLINE emptyPR_2 #-}
emptyPR_2 pra prb = P_2 0# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_2 #-}
replicatePR_2 pra prb n# p
  = P_2 n# (p `seq` replicatePR pra n# a)
           (p `seq` replicatePR prb n# b)
  where
    (a,b) = p

dPR_3 :: PR a -> PR b -> PR c -> PR (a,b,c)
{-# INLINE dPR_3 #-}
dPR_3 pra prb prc
  = PR {
      lengthPR    = lengthPR_3
    , emptyPR     = emptyPR_3 pra prb prc
    , replicatePR = replicatePR_3 pra prb prc
    }

{-# INLINE lengthPR_3 #-}
lengthPR_3 (P_3 n# _ _ _) = n#

{-# INLINE emptyPR_3 #-}
emptyPR_3 pra prb prc = P_3 0# (emptyPR pra) (emptyPR prb) (emptyPR prc)

{-# INLINE replicatePR_3 #-}
replicatePR_3 pra prb prc n# p
  = P_3 n# (p `seq` replicatePR pra n# a)
           (p `seq` replicatePR prb n# b)
           (p `seq` replicatePR prc n# c)
  where
    (a,b,c) = p


data Sum2 a b = Alt2_1 a | Alt2_2 b
data Sum3 a b c = Alt3_1 a | Alt3_2 b | Alt3_3 c

data instance PArray (Sum2 a b)
  = PSum2 Int# PArray_Int# PArray_Int# (PArray a)
                                      (PArray b)

data instance PArray (Sum3 a b c)
  = PSum3 Int# PArray_Int# PArray_Int# (PArray a)
                                       (PArray b)
                                       (PArray c)

dPR_Sum2 :: PR a -> PR b -> PR (Sum2 a b)
{-# INLINE dPR_Sum2 #-}
dPR_Sum2 pra prb = PR {
                     lengthPR    = lengthPR_Sum2
                   , emptyPR     = emptyPR_Sum2 pra prb
                   , replicatePR = replicatePR_Sum2 pra prb
                   }

{-# INLINE lengthPR_Sum2 #-}
lengthPR_Sum2 (PSum2 n# _ _ _ _) = n#

{-# INLINE emptyPR_Sum2 #-}
emptyPR_Sum2 pra prb
  = PSum2 0# emptyPA_Int# emptyPA_Int# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Sum2 #-}
replicatePR_Sum2 pra prb n# p
  = PSum2 n# (replicatePA_Int# n# (case p of Alt2_1 _ -> 1#
                                             Alt2_2 _ -> 2#))
             (upToPA_Int# n#)
             (case p of Alt2_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt2_2 y -> replicatePR prb n# y
                        _        -> emptyPR prb)

dPR_Sum3 :: PR a -> PR b -> PR c -> PR (Sum3 a b c)
{-# INLINE dPR_Sum3 #-}
dPR_Sum3 pra prb prc
  = PR {
     lengthPR    = lengthPR_Sum3
   , emptyPR     = emptyPR_Sum3 pra prb prc
   , replicatePR = replicatePR_Sum3 pra prb prc
   }

{-# INLINE lengthPR_Sum3 #-}
lengthPR_Sum3 (PSum3 n# _ _ _ _ _) = n#

{-# INLINE emptyPR_Sum3 #-}
emptyPR_Sum3 pra prb prc
  = PSum3 0# emptyPA_Int# emptyPA_Int# (emptyPR pra)
                                       (emptyPR prb)
                                       (emptyPR prc)

{-# INLINE replicatePR_Sum3 #-}
replicatePR_Sum3 pra prb prc n# p
  = PSum3 n# (replicatePA_Int# n# (case p of Alt3_1 _ -> 1#
                                             Alt3_2 _ -> 2#
                                             Alt3_3 _ -> 3#))
             (upToPA_Int# n#)
             (case p of Alt3_1 x -> replicatePR pra n# x
                        _        -> emptyPR pra)
             (case p of Alt3_2 x -> replicatePR prb n# x
                        _        -> emptyPR prb)
             (case p of Alt3_3 x -> replicatePR prc n# x
                        _        -> emptyPR prc)

