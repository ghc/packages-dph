
-- | Definition of the PRepr/PA family and class.
--
--   This module is kept separate from PRepr.Base to break an import cycle
--   between PRepr.Base PRepr.Instances and PArray.PData.Wrap
--
module Data.Array.Parallel.PArray.PRepr.Base (
        PRepr,
        PA (..),
        
        -- PD functions are the same as the methods of the PR class, 
        -- except that they take a PA dictinoary instead of a PR 
        -- dictionary.
        validPD,
        emptyPD,
        nfPD,
        lengthPD,
        replicatePD,    replicatesPD,
        indexPD,        indexlPD,
        extractPD,      extractsPD,
        appendPD,       appendsPD,
        packByTagPD,
        combine2PD,
        fromVectorPD,   toVectorPD,
        fromUArrayPD,   toUArrayPD
)
where
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted   as U
import Data.Vector                              (Vector)
import Data.Array.Parallel.Base                 (Tag)


-- | Representable types.
--
--   The family of types that we know how to represent generically.
--   PRepr takes an arbitrary type and produces the generic type we use to 
--   represent it.
--
--   Instances for simple types are defined in D.A.P.PArray.PRepr.Instances.
--   For algebraic types, it's up to the vectoriser/client module to create
--   a suitable instance.
--
type family PRepr a


-- | A PA dictionary contains the functions that we use to convert a
--   representable type to and from its generic representation.
--   The conversion methods should all be O(1).
class PR (PRepr a) => PA a where
  toPRepr      :: a -> PRepr a
  fromPRepr    :: PRepr a -> a
  toArrPRepr   :: PData a -> PData (PRepr a)
  fromArrPRepr :: PData (PRepr a) -> PData a


-- PD Wrappers ----------------------------------------------------------------
--  These wrappers work on (PData a) arrays when we know the element type 'a'
--  is representable. We implement them by converting the PData to the 
--  underlying representation type, and use the corresponding method from
--  the PR dictionary.
--
--  The wrappers are used in situations when we only have PA dictionary, 
--  instead of a PR dictionary. This happens in the PR (a :-> b) instance, 
--  as we need to work on a generically represented environment, and only
--  have an existential PA dictionary. We also use them in the PA functions
--  defined by D.A.P.PArray.
--
--  See the D.A.P.PArray.PData.Base for docs on what these functions do.
--
{-# INLINE validPD #-}
validPD         :: PA a => PData a -> Bool
validPD arr
 = validPR (toArrPRepr arr)


{-# INLINE_PA emptyPD #-}
emptyPD         :: PA a => PData a
emptyPD 
  = fromArrPRepr emptyPR


{-# INLINE_PA nfPD #-}
nfPD            :: PA a => PData a -> ()
nfPD arr
 = nfPR (toArrPRepr arr)


{-# INLINE_PA lengthPD #-}
lengthPD        :: PA a => PData a -> Int
lengthPD arr
 = lengthPR (toArrPRepr arr)
 

{-# INLINE_PA replicatePD #-}
replicatePD     :: PA a => Int -> a -> PData a
replicatePD n x
 = fromArrPRepr $ replicatePR n $ toPRepr x


{-# INLINE_PA replicatesPD #-}
replicatesPD    :: PA a => U.Segd -> PData a -> PData a
replicatesPD segd arr
 = fromArrPRepr $ replicatesPR segd (toArrPRepr arr)


{-# INLINE_PA indexPD #-}
indexPD         :: PA a => PData a    -> Int -> a
indexPD = undefined


{-# INLINE_PA indexlPD #-}
indexlPD        :: PA a => Int -> PData (PArray a) -> PData Int -> PData a
indexlPD = undefined


{-# INLINE_PA extractPD #-}
extractPD       :: PA a => PData a -> Int -> Int -> PData a
extractPD = undefined


{-# INLINE_PA extractsPD #-}
extractsPD      :: PA a => Vector (PData a) -> U.SSegd -> PData a
extractsPD = undefined


{-# INLINE_PA appendPD #-}
appendPD        :: PA a => PData a -> PData a -> PData a
appendPD = undefined


{-# INLINE_PA appendsPD #-}
appendsPD       :: PA a => U.Segd -> U.Segd -> PData a -> U.Segd -> PData a -> PData a
appendsPD = undefined


{-# INLINE_PA packByTagPD #-}
packByTagPD     :: PA a => PData a -> U.Array Tag -> Tag -> PData a
packByTagPD = undefined


{-# INLINE_PA combine2PD #-}
combine2PD      :: PA a => U.Sel2 -> PData a -> PData a -> PData a
combine2PD = undefined


{-# INLINE_PA fromVectorPD #-}
fromVectorPD  :: PA a => Vector a -> PData a
fromVectorPD = undefined


{-# INLINE_PA toVectorPD #-}
toVectorPD    :: PA a => PData a -> Vector a
toVectorPD   = undefined


{-# INLINE_PA fromUArrayPD #-}
fromUArrayPD  :: (PA a, U.Elt a) => U.Array a -> PData a
fromUArrayPD  = undefined


{-# INLINE_PA toUArrayPD #-}
toUArrayPD    :: (PA a, U.Elt a) => PData a -> U.Array a
toUArrayPD      = undefined
