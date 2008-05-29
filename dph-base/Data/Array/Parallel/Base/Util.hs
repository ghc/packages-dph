module Data.Array.Parallel.Base.Util (
  fromBool, toBool
) where

fromBool :: Num a => Bool -> a
fromBool False = 0
fromBool True  = 1
{-# INLINE fromBool #-}

toBool :: Num a => a -> Bool
toBool n | n == 0    = False
         | otherwise = True
{-# INLINE toBool #-}

