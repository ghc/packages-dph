
-- | Constructor tags.
module Data.Array.Parallel.Base.Util (
  Tag, fromBool, toBool, tagToInt, intToTag
) where


-- | Given a value of an algebraic type, the tag tells us what
--   data constructor was used to create it.
type Tag = Int


-- | Get the `Tag` of a `Bool` value. `False` is 0, `True` is 1.
{-# INLINE fromBool #-}
fromBool :: Bool -> Tag
fromBool False = 0
fromBool True  = 1


-- | Convert a `Tag` to a `Bool` value.
{-# INLINE toBool #-}
toBool :: Tag -> Bool
toBool n | n == 0    = False
         | otherwise = True


-- | Convert a `Tag` to an `Int`. This is identity at the value level.
{-# INLINE tagToInt #-}
tagToInt :: Tag -> Int
tagToInt = id

-- | Convert an `Int` to a `Tag`. This is identity at the value level.
{-# INLINE intToTag #-}
intToTag :: Int -> Tag
intToTag = id

