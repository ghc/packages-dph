module Solver
    (Solver,solvers)
where

import Common
import qualified Vector                         as SV
import qualified Vectorised                     as SPA
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import qualified Data.Array.Parallel	as P
import qualified Data.Array.Parallel.PArray	as P


type Solver = VU.Vector Vec3 -> VU.Vector (Int,Int,Int,Colour) -> VU.Vector Vec3 -> Double -> VU.Vector Colour

solvers :: [(String,Solver)]
solvers =
 [("vectorised", solverPA)
 ,("vector",     SV.solveV)]

solverPA verts tris rays time
 = let fv a = P.fromVector (VU.convert a)
   in  VU.convert (P.toVector (SPA.solvePA (fv verts) (fv tris) (fv rays) time))
