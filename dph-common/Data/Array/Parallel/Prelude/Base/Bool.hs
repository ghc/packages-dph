{-# LANGUAGE ParallelArrays #-}
module Data.Array.Parallel.Prelude.Base.Bool (
  andP, andPA, orP, orPA, notV, andV, orV
) where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PReprInstances
import Data.Array.Parallel.Lifted.Scalar
import qualified Data.Array.Parallel.Unlifted as U
import Data.Bits


andP:: [:Bool:] -> Bool
{-# NOINLINE andP #-}
andP _ = True

andPA:: PArray Bool :-> Bool
{-# INLINE andPA #-}
andPA = closure1 (scalar_fold (&&) True) 
                 (scalar_folds (&&) True) 


orP:: [:Bool:] -> Bool
{-# NOINLINE orP #-}
orP _ = True

orPA:: PArray Bool :-> Bool
{-# INLINE orPA #-}
orPA = closure1 (scalar_fold (||) False) 
                 (scalar_folds (||) False)

not_l :: PArray Bool -> PArray Bool
{-# INLINE not_l #-}
not_l (PArray n# bs)
  = PArray n#
  $ case bs of { PBool sel ->
    PBool $ U.tagsToSel2 (U.map complement (U.tagsSel2 sel)) }

notV :: Bool :-> Bool
{-# INLINE notV #-}
notV = closure1 not not_l

and_l :: PArray Bool -> PArray Bool -> PArray Bool
{-# INLINE and_l #-}
and_l (PArray n# bs) (PArray _ cs)
  = PArray n#
  $ case bs of { PBool sel1 ->
    case cs of { PBool sel2 ->
    PBool $ U.tagsToSel2 (U.zipWith (.&.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}

andV :: Bool :-> Bool :-> Bool
{-# INLINE andV #-}
andV = closure2 (&&) and_l

or_l :: PArray Bool -> PArray Bool -> PArray Bool
{-# INLINE or_l #-}
or_l (PArray n# bs) (PArray _ cs)
  = PArray n#
  $ case bs of { PBool sel1 ->
    case cs of { PBool sel2 ->
    PBool $ U.tagsToSel2 (U.zipWith (.|.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}

orV :: Bool :-> Bool :-> Bool
{-# INLINE orV #-}
orV = closure2 (||) or_l

