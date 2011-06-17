{-# OPTIONS_GHC -fvectorise #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.VectDepend
-- Copyright   :  (c) 2001-2011 The Data Parallel Haskell team
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Note [Vectoriser dependencies]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some of the modules in 'dph-common' (and hence, 'dph-seq' and 'dph-par') are being vectorised,
-- whereas some other modules in 'dph-common' define names hardwired into the vectoriser — i.e.,
-- the vectoriser panics if those latter modules have not been compiled yet.  To avoid build races
-- we need to ensure that all vectoriser dependencies are compiler before any of the vectorised
-- modules is being compiled.
--
-- The present module imports all vectoriser dependencies and we ensure that those are available
-- in vectorised modules by importing the present module into those vectorised modules.  In other
-- words, we turn the indirect module dependencies through the vectoriser into explicit module
-- dependencies, which the dependency tracker of the build system will correctly handle.

-- #hide
module Data.Array.Parallel.VectDepend () where

import Data.Array.Parallel.PArray.Base            ()
import Data.Array.Parallel.PArray.Scalar          ()
import Data.Array.Parallel.PArray.ScalarInstances ()
import Data.Array.Parallel.PArray.PRepr           ()
import Data.Array.Parallel.PArray.PReprInstances  ()
import Data.Array.Parallel.PArray.PData           ()
import Data.Array.Parallel.PArray.PDataInstances  ()
import Data.Array.Parallel.PArray.Types           ()
import Data.Array.Parallel.Lifted.Closure         ()
import Data.Array.Parallel.Lifted.Unboxed         ()
import Data.Array.Parallel.Lifted.Scalar          ()
import Data.Array.Parallel.Prelude.Tuple          ()
