{-# LANGUAGE ExistentialQuantification, FlexibleInstances,
             TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Stream.Flat.Stream
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (existentials)
--
--
-- Basic types for stream-based fusion
--

module Data.Array.Parallel.Stream.Flat.Stream (
  Step(..), Stream(..),

  SArgs(..), sNoArgs
) where

import Data.Array.Parallel.Base (
  Rebox)

data Step s a = Done
              | Skip     !s
              | Yield !a !s

instance Functor (Step s) where
  fmap f Done        = Done
  fmap f (Skip s)    = Skip s
  fmap f (Yield x s) = Yield (f x) s

data Stream a = forall s. Rebox s => Stream (s -> Step s a) !s Int String

sNoArgs :: String -> String
sNoArgs = id

class SArgs a where
  sArgs :: String -> a -> String

instance SArgs () where
  sArgs fn _ = fn

instance SArgs String where
  sArgs fn arg = fn ++ " <- " ++ arg

instance SArgs (String, String) where
  sArgs fn (arg1, arg2) = fn ++ " <- (" ++ arg1 ++ ", " ++ arg2 ++ ")"

instance SArgs (String, String, String) where
  sArgs fn (arg1, arg2, arg3)
    = fn ++ " <- (" ++ arg1 ++ ", " ++ arg2 ++ ", " ++ arg3 ++ ")"

instance SArgs (String, String, String, String) where
  sArgs fn (arg1, arg2, arg3, arg4)
    = fn ++ " <- (" ++ arg1 ++ ", " ++ arg2 ++ ", " ++ arg3 ++ ", " ++ arg4 ++ ")"

instance SArgs (String, String, String, String, String) where
  sArgs fn (arg1, arg2, arg3, arg4, arg5)
    = fn ++ " <- (" ++ arg1 ++ ", " ++ arg2 ++ ", " ++ arg3 ++ ", " ++ arg4 ++ ", " ++ arg5 ++ ")"

