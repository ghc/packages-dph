{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Bool 
        ( Bool(..)
        , otherwise
        , (&&), (||), not) --, andP, orP)
where
-- Primitives needed by the vectoriser.
import Data.Array.Parallel.Prim

import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted           as U
import Data.Bits
        

-- and ------------------------------------------------------------------------
{-# VECTORISE (&&) = (&&*) #-}

(&&*) :: Bool :-> Bool :-> Bool
(&&*) = closure2 (&&) and_l
{-# INLINE      (&&*) #-}
{-# NOVECTORISE (&&*) #-}

and_l :: PArray Bool -> PArray Bool -> PArray Bool
and_l (PArray n# bs) (PArray _ cs)
  = PArray n# $
      case bs of { PBool sel1 ->
      case cs of { PBool sel2 ->
      PBool $ U.tagsToSel2 (U.zipWith (.&.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}
{-# INLINE      and_l #-}
{-# NOVECTORISE and_l #-}


-- or -------------------------------------------------------------------------
{-# VECTORISE (||) = (||*) #-}

(||*) :: Bool :-> Bool :-> Bool
(||*) = closure2 (||) or_l
{-# INLINE (||*) #-}
{-# NOVECTORISE (||*) #-}

or_l :: PArray Bool -> PArray Bool -> PArray Bool
or_l (PArray n# bs) (PArray _ cs)
  = PArray n# $
      case bs of { PBool sel1 ->
      case cs of { PBool sel2 ->
      PBool $ U.tagsToSel2 (U.zipWith (.|.) (U.tagsSel2 sel1) (U.tagsSel2 sel2)) }}
{-# INLINE or_l #-}
{-# NOVECTORISE or_l #-}


-- not ------------------------------------------------------------------------
{-# VECTORISE not = notPP #-}

notPP :: Bool :-> Bool
notPP   = closure1 not notPP_l
{-# INLINE notPP #-}
{-# NOVECTORISE notPP #-}

notPP_l :: PArray Bool -> PArray Bool
notPP_l (PArray n# bs)
  = PArray n# $
      case bs of { PBool sel ->
      PBool $ U.tagsToSel2 (U.map complement (U.tagsSel2 sel)) }
{-# NOVECTORISE notPP_l #-}
{-# INLINE notPP_l #-}


{- TODO: We can't do these because there is no Unboxes instance for Bool.
-- andP -----------------------------------------------------------------------
andP :: PArr Bool -> Bool
andP _ = True
{-# NOINLINE  andP #-}
{-# VECTORISE andP = andPP #-}

andPP :: PArray Bool :-> Bool
andPP  = L.closure1' (SC.fold (&&) True) (SC.folds (&&) True)
{-# INLINE      andPP #-}
{-# NOVECTORISE andPP #-}


-- orP ------------------------------------------------------------------------
orP :: PArr Bool -> Bool
orP _ = True
{-# NOINLINE  orP #-}
{-# VECTORISE orP = orPP #-}

orPP :: PArray Bool :-> Bool
orPP   = L.closure1' (SC.fold (||) False) (SC.folds (||) False)
{-# INLINE      orPP #-}
{-# NOVECTORISE orPP #-}
-}