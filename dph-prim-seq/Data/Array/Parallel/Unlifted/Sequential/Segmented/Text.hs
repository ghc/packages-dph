-- | Read\/Show instances for segmented unlifted arrays.
module Data.Array.Parallel.Unlifted.Sequential.Segmented.Text ()
where
import Data.Array.Parallel.Base (
  Read(..), showsApp, readApp)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.USegd (
  USegd, lengthsUSegd )

instance Show USegd where
  showsPrec k = showsApp k "toUSegd" . lengthsUSegd

