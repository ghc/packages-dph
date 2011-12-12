{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- Distribution of Maybes
module Data.Array.Parallel.Unlifted.Distributed.Types.Maybe where
import Data.Array.Parallel.Unlifted.Distributed.Types.Prim      ()
import Data.Array.Parallel.Unlifted.Distributed.Types.Base
import Control.Monad

instance DT a => DT (Maybe a) where
  data Dist  (Maybe a)   = DMaybe  !(Dist  Bool)   !(Dist  a)
  data MDist (Maybe a) s = MDMaybe !(MDist Bool s) !(MDist a s)

  indexD str (DMaybe bs as) i
    |        indexD (str ++ "/indexD[Maybe]") bs i
    = Just $ indexD (str ++ "/indexD[Maybe]" ++ str) as i
    | otherwise           = Nothing

  newMD g
   = liftM2 MDMaybe (newMD g) (newMD g)

  readMD (MDMaybe bs as) i 
   = do b <- readMD bs i
        if b then liftM Just $ readMD as i
             else return Nothing

  writeMD (MDMaybe bs _) i Nothing 
   = writeMD bs i False

  writeMD (MDMaybe bs as) i (Just x)
   = do writeMD bs i True
        writeMD as i x

  unsafeFreezeMD (MDMaybe bs as)
   = liftM2 DMaybe (unsafeFreezeMD bs)
                   (unsafeFreezeMD as)

  {-# INLINE deepSeqD #-}
  deepSeqD Nothing  z   = z
  deepSeqD (Just x) z   = deepSeqD x z

  sizeD  (DMaybe  b _)  = sizeD  b
  sizeMD (MDMaybe b _)  = sizeMD b

  measureD Nothing      = "Nothing"
  measureD (Just x)     = "Just (" ++ measureD x ++ ")"
