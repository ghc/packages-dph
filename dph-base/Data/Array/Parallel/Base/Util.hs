module Data.Array.Parallel.Base.Util (
  fromBool, toBool
) where

fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1
{-# INLINE fromBool #-}

toBool :: Int -> Bool
toBool n | n == 0    = False
         | otherwise = True
{-# INLINE toBool #-}

