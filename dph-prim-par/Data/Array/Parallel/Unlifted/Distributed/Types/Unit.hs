{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Distribution of unit values.
module Data.Array.Parallel.Unlifted.Distributed.Types.Unit 
        (unitD)
where
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Base

here :: String -> String
here s = "Data.Array.Parallel.Unlifted.Distributed.Types.Unit." ++ s

instance DT () where
  data Dist ()    = DUnit  !Int
  data MDist () s = MDUnit !Int

  indexD str (DUnit n) i
   = check (str ++ "/indexD[Unit]") n i
   $  ()

  newMD
   = return . MDUnit . gangSize

  readMD   (MDUnit n) i
   = check (here "readMD")  n i
   $ return ()

  writeMD  (MDUnit n) i ()
   = check (here "writeMD") n i
   $ return ()

  unsafeFreezeMD (MDUnit n)
   = return $ DUnit n

  sizeD  = error $ here "sizeD  undefined"
  sizeMD = error $ here "sizeMD undefined"


-- | Yield a distributed unit.
unitD :: Gang -> Dist ()
unitD = DUnit . gangSize
{-# INLINE_DIST unitD #-}
