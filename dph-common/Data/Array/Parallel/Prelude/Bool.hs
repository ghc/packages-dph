{-# OPTIONS -fvectorise #-}
module Data.Array.Parallel.Prelude.Bool (
  Bool(..),

  otherwise, (&&), (||), not, andP, orP
) where

-- import Data.Array.Parallel.Prelude.Base
import Data.Array.Parallel.Prelude.Base.Bool

-- import qualified Prelude as P
import Prelude (Bool(..), (&&), (||), not)

otherwise :: Bool
otherwise = True

