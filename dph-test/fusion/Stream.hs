{-# LANGUAGE FlexibleContexts #-}
module Stream where

import Data.Vector.Generic               as G
import Data.Vector.Generic.Base          as G
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Stream.Size    as S
import Data.Vector.Fusion.Util           as S


-- Locked Streamers -----------------------------------------------------------

-- | Stream two things.
lockedStream2 
        :: (Monad m, Vector v a, Vector v b)
        => v a -> v b 
        -> Stream m (a, b)

lockedStream2 aa bb
 = aa `seq` bb `seq` n `seq` (S.unfoldr get 0 `S.sized` Exact n)
 where  n        = G.length aa

        {-# INLINE [0] get #-}
        get i   
         | i >= n       = Nothing

         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         = Just ((a, b), i + 1)
{-# INLINE [1] lockedStream2 #-}


-- | Stream three things.
lockedStream3
        :: (Monad m, Vector v a, Vector v b, Vector v c)
        => v a -> v b -> v c 
        -> Stream m (a, b, c)

lockedStream3 aa bb cc
 = aa `seq` bb `seq` cc
 `seq` n `seq` (S.unfoldr get 0 `S.sized` Exact n)
 where  n        = G.length aa

        {-# INLINE [0] get #-}
        get i   
         | i >= n       = Nothing

         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         , Box c        <- basicUnsafeIndexM cc i
         = Just ((a, b, c), i + 1)
{-# INLINE [1] lockedStream3 #-}


-- Locked Stream Zippers -----------------------------------------------------
lockedZip2S 
        :: Monad m 
        => Stream m a -> Stream m b 
        -> Stream m (a, b)

lockedZip2S (Stream mkStep1 sa1 size1)
            (Stream mkStep2 sa2 _)
 = Stream step (sa1, sa2) size1
 where 
        {-# INLINE [0] step #-}
        step (s1, s2)
         = do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                return $ case (step1, step2) of
                          (Yield x1 s1', Yield x2 s2')  -> Yield (x1, x2) (s1', s2')
                          _                             -> Done
{-# INLINE [1] lockedZip2S #-}


lockedZip3S 
        :: Monad m 
        => Stream m a -> Stream m b -> Stream m c
        -> Stream m (a, b, c)

lockedZip3S (Stream mkStep1 sa1 size1)
            (Stream mkStep2 sa2 _)
            (Stream mkStep3 sa3 _)
 = Stream step (sa1, sa2, sa3) size1
 where 
        {-# INLINE [0] step #-}
        step (s1, s2, s3)
         = do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                return $ case (step1, step2, step3) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3')  
                            -> Yield (x1, x2, x3) (s1', s2', s3')

                          _ -> Done
{-# INLINE [1] lockedZip3S #-}


{-}
lockedZip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
lockedZip3 aa bb cc 
 = unstream $ lockedStream3 aa bb cc

lockedZip3S :: Stream a -> Stream a -> Stremam a -> Stream (a, b, c)
lockedZip3S

thing aa bb cc
 = lockedZip3 aa (map double bb) cc

 = unstream $ lockedStream aa (unstream $ replicateS len aa) cc

 = unstream $ lockedZip2S (lockedStream aa cc) (replicateS len aa)

 If we see an unstream as one of the arguments of a 'lockedStreamN' then 
 use rewrite rules to rotate it away and treat that as a separate stream.


-}

 
