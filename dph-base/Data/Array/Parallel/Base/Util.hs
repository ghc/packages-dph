module Data.Array.Parallel.Base.Util (
  Tag, fromBool, toBool, tagToInt, intToTag
) where

import Data.Word ( Word8 )

type Tag = Int

fromBool :: Bool -> Tag
fromBool False = 0
fromBool True  = 1
{-# INLINE fromBool #-}

toBool :: Tag -> Bool
toBool n | n == 0    = False
         | otherwise = True
{-# INLINE toBool #-}

tagToInt :: Tag -> Int
tagToInt = id -- fromEnum
{-# INLINE tagToInt #-}

intToTag :: Int -> Tag
intToTag = id -- toEnum
{-# INLINE intToTag #-}

