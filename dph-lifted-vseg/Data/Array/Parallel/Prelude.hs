{-# OPTIONS_GHC -fvectorise #-}

-- | This module (as well as the type-specific modules
--  'Data.Array.Parallel.Prelude.*') are a temporary kludge needed as DPH
--  programs cannot directly use the (non-vectorised) functions from the
--  standard Prelude.  It also exports some conversion helpers.
--
--  /This module should not be explicitly imported in user code anymore./
--   User code should only import 'Data.Array.Parallel' and, until the
--  vectoriser supports type classes, the type-specific
--  modules 'Data.Array.Parallel.Prelude.*'.
module Data.Array.Parallel.Prelude 
        ( module Data.Array.Parallel.Prelude.Bool
        , module Data.Array.Parallel.Prelude.Tuple
        , PArray, Scalar(..))
where
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Tuple
import Data.Array.Parallel.PArray.Scalar
import Data.Array.Parallel.PArray        

{-# VECTORISE type () = () #-}
