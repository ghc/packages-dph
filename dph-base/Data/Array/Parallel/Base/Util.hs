module Data.Array.Parallel.Base.Util (
  Tag, fromBool, toBool, tagToInt, intToTag
) where

import Data.Word ( Word8 )

type Tag = Word8

fromBool :: Bool -> Tag
fromBool False = 0
fromBool True  = 1
{-# INLINE fromBool #-}

toBool :: Tag -> Bool
toBool n | n == 0    = False
         | otherwise = True
{-# INLINE toBool #-}

tagToInt :: Tag -> Int
tagToInt = fromEnum
{-# INLINE tagToInt #-}

intToTag :: Int -> Tag
intToTag = toEnum
{-# INLINE intToTag #-}

