{-# OPTIONS_HADDOCK hide #-}

-- |This modules defines the interface between the DPH libraries and the compiler.  In particular,
-- it exports exactly those definitions that are used by either the desugarer (to remove parallel array
-- syntax) or by the vectoriser (to generate vectorised code).
--
-- The DPH libraries can evolve between compiler releases as long as this interface remains the same.
--
-- WARNING: All modules in this package that need to be vectorised (i.e., are compiled with
--          '-fvectorise' must directly or indirectly import this module).  This is to ensure that the
--          build system does not attempt to compile a vectorised module before all definitions that are
--          required by the vectoriser are available.

-- #hide
module Data.Array.Parallel.Prim (
  PArray(..), PData, PRepr, PA(..), PR(..),
  replicatePD, emptyPD, packByTagPD, combine2PD,
  Scalar(..),
  scalar_map, scalar_zipWith, scalar_zipWith3,
  Void, Sum2(..), Sum3(..), Wrap(..),
  void, fromVoid, pvoid, punit,
  (:->)(..), 
  closure, liftedClosure, ($:), liftedApply, closure1, closure2, closure3,
  Sel2,
  replicateSel2#, tagsSel2, elementsSel2_0#, elementsSel2_1#,
  PArray_Int#, PArray_Double#,
  replicatePA_Int#, replicatePA_Double#,
  emptyPA_Int#, emptyPA_Double#,
  packByTagPA_Int#, packByTagPA_Double#,
  combine2PA_Int#, combine2PA_Double#,

  tup2, tup3
) where

-- We use explicit import lists here to make the vectoriser interface explicit and keep it under tight
-- control.
--
import Data.Array.Parallel.PArray.Base            (PArray(..))
import Data.Array.Parallel.PArray.Scalar          (Scalar(..))
import Data.Array.Parallel.PArray.ScalarInstances ( {-we requires instances-} )
import Data.Array.Parallel.PArray.PRepr           (PRepr, PA(..), replicatePD, emptyPD, packByTagPD,
                                                   combine2PD)
import Data.Array.Parallel.PArray.Types           (Void, Sum2(..), Sum3(..), Wrap(..), void, fromVoid)
import Data.Array.Parallel.PArray.PReprInstances  ( {-we requires instances-} )
import Data.Array.Parallel.PArray.PData           (PData, PR(..))
import Data.Array.Parallel.PArray.PDataInstances  (pvoid, punit)
import Data.Array.Parallel.Lifted.Closure         ((:->)(..), closure, liftedClosure, ($:), liftedApply,
                                                   closure1, closure2, closure3)
import Data.Array.Parallel.Lifted.Unboxed         (Sel2, replicateSel2#, tagsSel2, elementsSel2_0#,
                                                   elementsSel2_1#,
                                                   PArray_Int#, PArray_Double#, 
                                                   replicatePA_Int#, replicatePA_Double#,
                                                   emptyPA_Int#, emptyPA_Double#,
                                                   {- packByTagPA_Int#, packByTagPA_Double# -}
                                                   combine2PA_Int#, combine2PA_Double#)
import Data.Array.Parallel.Lifted.Scalar          (scalar_map, scalar_zipWith, scalar_zipWith3)
import Data.Array.Parallel.Prelude.Tuple          (tup2, tup3)


packByTagPA_Int#, packByTagPA_Double# :: a
packByTagPA_Int#    = error "Data.Array.Parallel.Prim: 'packByTagPA_Int#' not implemented"
packByTagPA_Double# = error "Data.Array.Parallel.Prim: 'packByTagPA_Double#' not implemented"
