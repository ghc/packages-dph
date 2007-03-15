module SMVMPar
where

import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted

type SparseMatrix = SUArr (Int :*: Double)
type SparseVector = UArr (Int :*: Double)
type Vector       = UArr Double

smvm :: SparseMatrix -> Vector -> Vector
smvm sm v = sumSUP (zipWithSUP (*) (bpermuteSUP' v (fstSU sm)) (sndSU sm))

