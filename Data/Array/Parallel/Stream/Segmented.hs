-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Segmented
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
-- Description ---------------------------------------------------------------
--
-- Segmented streams. This module will go away. 
--

module Data.Array.Parallel.Stream.Segmented (
  SStream(..),
  segmentS, foldValuesSS,
  combineSS
) where

import Data.Array.Parallel.Base (
  (:*:)(..), Box(..), MaybeS(..))
import Data.Array.Parallel.Stream.Flat (
  Step(..), Stream(..))

data SStream a = SStream { segd   :: Stream Int
                         , values :: Stream a
                         }

segmentS :: Stream Int -> Stream a -> SStream a
{-# INLINE segmentS #-}
segmentS = SStream
        
foldValuesSS :: (a -> b -> a) -> a -> SStream b -> Stream a
{-# INLINE [1] foldValuesSS #-}
foldValuesSS f z  (SStream (Stream nexts ss ns) (Stream nextv vs nv)) =
  Stream next (NothingS :*: Box z :*: ss :*: vs) ns
  where
    {-# INLINE next #-}
    next (NothingS :*: Box x :*: ss :*: vs) =
      case nexts ss of
        Done        -> Done
        Skip    ss' -> Skip (NothingS :*: Box x :*: ss' :*: vs)
        Yield n ss' -> Skip (JustS n  :*: Box z :*: ss' :*: vs)

    next (JustS 0 :*: Box x :*: ss :*: vs) =
      Yield x (NothingS :*: Box z :*: ss :*: vs)
    next (JustS n :*: Box x :*: ss :*: vs) =
      case nextv vs of
        Done        -> Done
                       -- FIXME
                       -- error
                       --  "Stream.Segmented.foldSS: invalid segment descriptor"
        Skip    vs' -> Skip (JustS n :*: Box x :*: ss :*: vs')
        Yield y vs' -> Skip (JustS (n-1) :*: Box (f x y) :*: ss :*: vs')



combineSS:: (Stream Bool) -> SStream a -> SStream a -> Stream a
{-# INLINE [1] combineSS #-}
combineSS (Stream nextf sf nf) 
            (SStream (Stream nexts1 ss1 ns1) (Stream nextv1 vs1 nv1))
            (SStream (Stream nexts2 ss2 ns2) (Stream nextv2 vs2 nv2)) =
  Stream next (NothingS :*: Box True :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2) (nv1+nv2)
  where
    {-# INLINE next #-}
    next (NothingS :*: f :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2) =
       case nextf sf of
         Done        -> Done
         Skip sf'    -> Skip (NothingS :*: f :*: sf' :*: ss1 :*: vs1 :*: ss2 :*: vs2) 
         Yield c sf' -> if c
                          then case nexts1 ss1 of
                                 Done         -> Done
                                 Skip ss1'    -> Skip (NothingS :*: f :*: sf :*: ss1' :*: vs1 :*: ss2 :*: vs2) 
                                 Yield n ss1' -> Skip (JustS n  :*: Box c :*: sf' :*: ss1' :*: vs1 :*: ss2 :*: vs2) 
                          else  case nexts2 ss2 of
                                 Done         -> Done
                                 Skip ss2'    -> Skip (NothingS :*: f :*: sf :*: ss1 :*: vs1 :*: ss2' :*: vs2) 
                                 Yield n ss2' -> Skip (JustS n  :*: Box c :*: sf' :*: ss1 :*: vs1 :*: ss2' :*: vs2) 
    next (JustS 0 :*: _ :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2) =                                            
         Skip (NothingS :*: Box True :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2)
    next (JustS n :*: Box True :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2) =                                            
      case nextv1 vs1 of
        Done         -> Done
        Skip vs1'    -> Skip (JustS n          :*: Box True :*: sf :*: ss1 :*: vs1' :*: ss2 :*: vs2) 
        Yield x vs1' -> Yield x (JustS (n-1)   :*: Box True :*: sf :*: ss1 :*: vs1' :*: ss2 :*: vs2) 
    next (JustS n :*: Box False :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2) =                                            
      case nextv2 vs2 of
        Done         -> Done
        Skip vs2'    -> Skip (JustS n        :*: Box False :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2') 
        Yield x vs2' -> Yield x (JustS (n-1) :*: Box False :*: sf :*: ss1 :*: vs1 :*: ss2 :*: vs2') 