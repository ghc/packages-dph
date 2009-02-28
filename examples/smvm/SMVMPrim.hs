module SMVMPrim
where

import Data.Array.Parallel.Unlifted as U

type SparseMatrix = U.SArray (Int :*: Double)
type SparseVector = U.Array (Int :*: Double)
type Vector       = U.Array Double

smvm :: SparseMatrix -> Vector -> Vector
smvm sm v = U.sum_s (U.zipWith_s (*) (bpermuteSUP' v (fstSU sm)) (sndSU sm))
                                       ^^^??          ^^??        ^^??

