{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.Array.Parallel.PArray.PRepr
        (PA)
where
import Data.Array.Parallel.PArray.PData

-- | In the existing dph-common package, the PA dictionary contains functions
--   to convert user-defined types to their generic representations, but we
--   haven't done that yet. This class is just a place holder so we get a
--   PA constraint like in the existing package.
class PR a => PA a

instance PR a => PA a