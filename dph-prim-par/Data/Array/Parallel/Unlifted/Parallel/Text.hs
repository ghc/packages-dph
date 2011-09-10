-- | Read\/Show instances for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Parallel.Text ()
where

import Data.Array.Parallel.Base (
  showsApp)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd (
  UPSegd, lengthsUPSegd )

instance Show UPSegd where
  showsPrec k = showsApp k "toUPSegd" . lengthsUPSegd