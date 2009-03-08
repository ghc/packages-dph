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
--
-- Segmented streams. This module will go away. 
--

{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Stream.Segmented (
  SStream(..),
  segmentS, foldValuesSS, fold1ValuesSS,
  combineSS, (^+++^)
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
{-# INLINE_STREAM foldValuesSS #-}
foldValuesSS f z (SStream is xs) = foldValuesSS' f z is xs

-- We don't want the case to appear in the simplifier output before the
-- INLINE_STREAM phase. That's why the rule.
--
{-# RULES

"foldValuesSS/SStream" forall f z is xs.
  foldValuesSS f z (SStream is xs) = foldValuesSS' f z is xs

  #-}
        
foldValuesSS' :: (a -> b -> a) -> a -> Stream Int -> Stream b -> Stream a
{-# INLINE_STREAM foldValuesSS' #-}
foldValuesSS' f z (Stream nexts ss ns) (Stream nextv vs nv) =
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

fold1ValuesSS :: (a -> a -> a) -> SStream a -> Stream a
{-# INLINE_STREAM fold1ValuesSS #-}
fold1ValuesSS f (SStream (Stream nexts ss ns) (Stream nextv vs nv)) =
  Stream next (NothingS :*: NothingS :*: ss :*: vs) ns
  where
    {-# INLINE next #-}
    next (NothingS :*: _ :*: ss :*: vs) =
      case nexts ss of
        Done        -> Done
        Skip    ss' -> Skip (NothingS :*: NothingS :*: ss' :*: vs)
        Yield n ss' -> Skip (JustS n  :*: NothingS :*: ss' :*: vs)

    next (JustS n :*: NothingS :*: ss :*: vs) =
      case nextv vs of
        Done        -> Done -- FIXME: error
        Skip    vs' -> Skip (JustS n     :*: NothingS      :*: ss :*: vs')
        Yield x vs' -> Skip (JustS (n-1) :*: JustS (Box x) :*: ss :*: vs')

    next (JustS 0 :*: JustS (Box x) :*: ss :*: vs) =
      Yield x (NothingS :*: NothingS :*: ss :*: vs)

    next (JustS n :*: JustS (Box x) :*: ss :*: vs) =
      case nextv vs of
        Done        -> Done  -- FIXME: error
        Skip    vs' -> Skip (JustS n     :*: JustS (Box x)        :*: ss :*: vs')
        Yield y vs' -> Skip (JustS (n-1) :*: JustS (Box (f x  y)) :*: ss :*: vs')


combineSS:: (Stream Bool) -> SStream a -> SStream a -> Stream a
{-# INLINE_STREAM combineSS #-}
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

(^+++^) :: SStream a -> SStream a -> Stream a
{-# INLINE_STREAM (^+++^) #-}
SStream (Stream nexts1 ss1 ns1) (Stream nextv1 sv1 nv1)
  ^+++^ SStream (Stream nexts2 ss2 ns2) (Stream nextv2 sv2 nv2)
  = Stream next (True :*: NothingS :*: ss1 :*: sv1 :*: ss2 :*: sv2) (nv1 + nv2)
  where
    {-# INLINE next #-}
    next (True :*: NothingS :*: ss1 :*: sv1 :*: ss2 :*: sv2)
      = case nexts1 ss1 of
          Done         -> Done
          Skip    ss1' -> Skip (True :*: NothingS
                                     :*: ss1' :*: sv1 :*: ss2 :*: sv2)
          Yield n ss1' -> Skip (True :*: JustS n
                                     :*: ss1' :*: sv1 :*: ss2 :*: sv2)

    next (True :*: JustS 0 :*: ss1 :*: sv1 :*: ss2 :*: sv2)
      = Skip (False :*: NothingS :*: ss1 :*: sv1 :*: ss2 :*: sv2)

    next (True :*: JustS n :*: ss1 :*: sv1 :*: ss2 :*: sv2)
      = case nextv1 sv1 of
          Done         -> Done  -- FIXME: error
          Skip    sv1' -> Skip    (True :*: JustS n
                                        :*: ss1 :*: sv1' :*: ss2 :*: sv2)
          Yield x sv1' -> Yield x (True :*: JustS (n-1)
                                        :*: ss1 :*: sv1' :*: ss2 :*: sv2)

    next (False :*: NothingS :*: ss1 :*: sv1 :*: ss2 :*: sv2)
      = case nexts2 ss2 of
          Done         -> Done  -- FIXME: error
          Skip    ss2' -> Skip (False :*: NothingS
                                      :*: ss1 :*: sv1 :*: ss2' :*: sv2)
          Yield n ss2' -> Skip (False :*: JustS n
                                      :*: ss1 :*: sv1 :*: ss2' :*: sv2)

    next (False :*: JustS 0 :*: ss1 :*: sv1 :*: ss2 :*: sv2)
      = Skip (True :*: NothingS :*: ss1 :*: sv1 :*: ss2 :*: sv2)

    next (False :*: JustS n :*: ss1 :*: sv1 :*: ss2 :*: sv2)
      = case nextv2 sv2 of
          Done         -> Done  -- FIXME: error
          Skip    sv2' -> Skip    (False :*: JustS n
                                         :*: ss1 :*: sv1 :*: ss2 :*: sv2')
          Yield x sv2' -> Yield x (False :*: JustS (n-1)
                                         :*: ss1 :*: sv1 :*: ss2 :*: sv2')

