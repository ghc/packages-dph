{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Sequential operations on unlifted arrays.
-- 
--   * This is an internal API and shouldn't need to be used directly.
--     Client programs should use "Data.Array.Parallel.Unlifted"
module Data.Array.Parallel.Unlifted.Sequential
        ( 
        -- * Constructors
          replicateSU
        , replicateRSU
        , appendSU
        , indicesSU,    indicesSU'

        -- * Folds
        , foldSU,       foldSSU
        , foldlSU,      foldlSSU,   foldlRU
        , foldl1SU,     foldl1SSU
        , fold1SU,      fold1SSU
        
        -- * Sums
        , andSU
        , orSU
        , sumSU,         sumRU
        , productSU
        , maximumSU
        , minimumSU

        -- * Pack and Combine
        , combineSU)
where
import Data.Array.Parallel.Unlifted.Sequential.Basics
import Data.Array.Parallel.Unlifted.Sequential.Combinators
import Data.Array.Parallel.Unlifted.Sequential.Sums
import Data.Array.Parallel.Unlifted.Sequential.USegd            ()
import Data.Array.Parallel.Unlifted.Sequential.USel             ()
import Data.Array.Parallel.Unlifted.Sequential.USSegd           (USSegd)
import Data.Array.Parallel.Unlifted.Sequential.UVSegd           ()
import Data.Array.Parallel.Unlifted.Sequential.Vector           as U
import qualified Data.Array.Parallel.Unlifted.Sequential.USegd  as USegd
import qualified Data.Vector                                    as V
import Prelude hiding (zip)
