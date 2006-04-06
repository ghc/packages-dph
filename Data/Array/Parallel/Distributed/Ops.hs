-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed.Ops
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Parallel operations on distributed types.
--


module Data.Array.Parallel.Distributed.Ops (
  replicateDT, mapDT, foldDT, scanDT
) where

import Data.Array.Parallel.Distributed.Types
import Data.Array.Parallel.Distributed.Gang
import Data.Array.Parallel.Base.Hyperstrict ( HS, (:*:)(..) )

import Data.Bits
import Control.Monad.ST                     ( ST, runST )
import Monad                                ( zipWithM )

here s = "Distributed.Ops." ++ s

-- | Yields a 'Dist' with the same value in every position.
replicateDT :: MDT a => Gang -> a -> ST s (Dist a)
replicateDT g = runDST g . return

-- | Maps a distributed value in parallel.
mapDT :: (DT a, MDT b) => Gang -> (a -> b) -> Dist a -> ST s (Dist b)
mapDT g f d = checkGangDT (here "mapDT") g d $
              runDST g (localDT d >>= return . f)

-- | Folds a distributed value.
--
-- /TODO:/ The current implementation is sequential.
foldDT :: MDT a => Gang -> (a -> a -> a) -> Dist a -> ST s a
foldDT g f d = checkGangDT (here "foldDT") g d $
               return (fold 1 $! (d `indexDT` 0))
  where
    fold i x | i == gangSize g = x
             | otherwise       = fold (i+1) $! f x (d `indexDT` i)

-- | Scans a distributed value, yielding the result of the scan and the sum of
-- all elements.
--
-- /TODO:/ The current implementation is sequential.
scanDT :: MDT a => Gang -> (a -> a -> a) -> Dist a -> ST s (Dist a :*: a)
scanDT g f d = checkGangDT (here "scanDT") g d $
               do
                 md <- newMDT g
                 let x = d `indexDT` 0
                 writeMDT md 0 (d `indexDT` 0)
                 y <- scan md 1 x 
                 r <- freezeMDT md
                 return (r :*: y)
  where
    scan md i x | i == gangSize g = return x
                | otherwise       = let y = f x (d `indexDT` i)
                                    in
                                    do
                                      writeMDT md i y
                                      scan md (i+1) y

-- For testing only                 
mkDT :: MDT a => Gang -> [a] -> Dist a
mkDT g xs = runST (do
                     mdt <- newMDT g
                     zipWithM (writeMDT mdt) [0 .. gangSize g - 1] xs
                     freezeMDT mdt)

