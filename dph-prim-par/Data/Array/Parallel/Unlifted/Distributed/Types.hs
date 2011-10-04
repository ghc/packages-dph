{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

-- | Distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Types (
        module Data.Array.Parallel.Unlifted.Distributed.Types.Vector,
        module Data.Array.Parallel.Unlifted.Distributed.Types.Maybe,
        module Data.Array.Parallel.Unlifted.Distributed.Types.Tuple,
        module Data.Array.Parallel.Unlifted.Distributed.Types.Prim,
        module Data.Array.Parallel.Unlifted.Distributed.Types.Unit,
        module Data.Array.Parallel.Unlifted.Distributed.Types.Base
) where
import Data.Array.Parallel.Unlifted.Distributed.Types.Vector
import Data.Array.Parallel.Unlifted.Distributed.Types.Maybe
import Data.Array.Parallel.Unlifted.Distributed.Types.Tuple
import Data.Array.Parallel.Unlifted.Distributed.Types.Prim
import Data.Array.Parallel.Unlifted.Distributed.Types.Unit
import Data.Array.Parallel.Unlifted.Distributed.Types.Base


