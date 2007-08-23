module Data.Array.Parallel.Lifted.PArray (
  PArray(..),

  PA(..),
  emptyPA,

  PRepr, PR(..),
  (:*:)(..), (:+:)(..), Embed(..),

  dPR_Unit, dPR_Cross, dPR_Plus, dPR_Embed,
  dPA_Unit
) where

import Data.Array.Parallel.Unlifted ( UArr, emptyU, replicateU )
import GHC.Exts (Int#, Int(..))

-- |Lifted parallel arrays
--
data family PArray a

-- |Representation types
--
type family PRepr a

-- |Dictionaries
--

data PA a = PA {
              lengthPA    :: PArray a -> Int#
            , replicatePA :: Int# -> a -> PArray a
            , toPRepr     :: a -> PRepr a
            , fromPRepr   :: PRepr a -> a
            , dictPRepr   :: PR (PRepr a)
            }

emptyPA :: PA a -> PArray a
emptyPA pa = replicatePA pa 0# (error "PArray.emptyPA: empty")

infixl 2 :*:
infixl 1 :+:

data a :*: b = a :*: b
data a :+: b = Inl a | Inr b

data Embed a = Embed a

data instance PArray ()        = PUnit  Int# ()
data instance PArray (a :*: b) = PCross Int# (PArray a) (PArray b)
data instance PArray (a :+: b) = PPlus  Int# (UArr Bool) (PArray a) (PArray b)
data instance PArray (Embed a) = PEmbed (PArray a)

data PR a = PR {
              lengthPR    :: PArray a -> Int#
            , emptyPR     :: PArray a
            , replicatePR :: Int# -> a -> PArray a
            }

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

type instance PRepr () = ()

dPA_Unit :: PA ()
{-# INLINE dPA_Unit #-}
dPA_Unit = PA {
             lengthPA    = lengthPR_Unit
           , replicatePA = replicatePR_Unit
           , toPRepr     = id
           , fromPRepr   = id
           , dictPRepr   = dPR_Unit
           }


dPR_Cross :: PR a -> PR b -> PR (a :*: b)
{-# INLINE dPR_Cross #-}
dPR_Cross pra prb = PR {
                      lengthPR    = lengthPR_Cross
                    , emptyPR     = emptyPR_Cross pra prb
                    , replicatePR = replicatePR_Cross pra prb
                    }

{-# INLINE lengthPR_Cross #-}
lengthPR_Cross (PCross n# _ _) = n#

{-# INLINE emptyPR_Cross #-}
emptyPR_Cross pra prb = PCross 0# (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Cross #-}
replicatePR_Cross pra prb n# p
  = PCross n# (p `seq` replicatePR pra n# a)
              (p `seq` replicatePR prb n# b)
  where
    a :*: b = p


dPR_Plus :: PR a -> PR b -> PR (a :+: b)
{-# INLINE dPR_Plus #-}
dPR_Plus pra prb = PR {
                     lengthPR    = lengthPR_Plus
                   , emptyPR     = emptyPR_Plus pra prb
                   , replicatePR = replicatePR_Plus pra prb
                   }

{-# INLINE lengthPR_Plus #-}
lengthPR_Plus (PPlus n# sel _ _) = n#

{-# INLINE emptyPR_Plus #-}
emptyPR_Plus pra prb = PPlus 0# emptyU (emptyPR pra) (emptyPR prb)

{-# INLINE replicatePR_Plus #-}
replicatePR_Plus pra prb n# p
  = PPlus n# (replicateU (I# n#) (case p of Inl _ -> True
                                            Inr _ -> False))
             (case p of Inl x -> replicatePR pra n# x
                        _     -> emptyPR pra)
             (case p of Inr y -> replicatePR prb n# y
                        _     -> emptyPR prb)

dPR_Embed :: PA a -> PR (Embed a)
{-# INLINE dPR_Embed #-}
dPR_Embed pa = PR {
                 lengthPR    = lengthPR_Embed pa
               , emptyPR     = emptyPR_Embed pa
               , replicatePR = replicatePR_Embed pa
               }

{-# INLINE lengthPR_Embed #-}
lengthPR_Embed pa (PEmbed xs) = lengthPA pa xs

{-# INLINE emptyPR_Embed #-}
emptyPR_Embed pa = PEmbed (emptyPA pa)

{-# INLINE replicatePR_Embed #-}
replicatePR_Embed pa n# (Embed x) = PEmbed (replicatePA pa n# x)

