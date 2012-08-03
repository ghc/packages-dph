{-# LANGUAGE FlexibleContexts #-}
module Stream where

import Data.Vector.Generic               as G
import Data.Vector.Generic.Base          as G
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Stream.Size    as S
import Data.Vector.Fusion.Util           as S
import qualified Data.Vector.Generic.New as New
import           Data.Vector.Generic.New ( New )


-- Swallow --------------------------------------------------------------------
-- | Like 'stream' but something else in the context knows how long it is.
--   We just keep returning elements and don't check for the end-of-vector
--   condition.
swallow :: Monad m => Vector v a => v a -> Stream m a
swallow v 
 = v `seq` n `seq` (S.unfoldr get 0 `S.sized` Unknown)
 where
        n   = G.length v

        {-# INLINE get #-}
        get i       
         | Box a        <- basicUnsafeIndexM v i 
         = Just (a, i + 1)
{-# INLINE [1] swallow #-}


swallowS stream = stream
{-# INLINE [1] swallowS #-}

{-# RULES "swallow/new/unstream"
    forall s
    . swallow (new (New.unstream s)) = swallowS s
  #-}


-- Repeat ---------------------------------------------------------------------
repeatM :: Monad m => m a -> Stream m a
repeatM x
 = Stream step () Unknown
 where
        {-# INLINE [0] step #-}
        step _
         = do   v <- x
                return $ Yield v ()

{-# INLINE [1] repeatM #-}


{-# RULES "swallowS/replicate"
    forall len x
    . swallowS (S.replicateM len x) = repeatM x
    #-}


-- Locked Streamers -----------------------------------------------------------
lockedStream2 
        :: (Monad m, Vector v a, Vector v b)
        => v a -> v b
        -> Stream m (a, b)

lockedStream2 aa bb
 = lockedZip2S (G.length aa) (swallow aa) (swallow bb)
{-# INLINE [1] lockedStream2 #-}


-- | Stream three things.
lockedStream3
        :: (Monad m, Vector v a, Vector v b, Vector v c)
        => v a -> v b -> v c 
        -> Stream m (a, b, c)

lockedStream3 aa bb cc
 = lockedZip3S (G.length aa) (swallow aa) (swallow bb) (swallow cc)
{-# INLINE [1] lockedStream3 #-}


{-# RULES "lockedStream3/new_1"
    forall as bs cs
    . lockedStream3 (G.new as) bs cs
    = S.map (\((b, c), a) -> (a, b, c))
    $ lockedZip2S (G.length bs) (lockedSwallow2 bs cs) (swallow (G.new as))
  #-}

{-# RULES "lockedStream3/new_2"
    forall as bs cs
    . lockedStream3 as (G.new bs) cs
    = S.map (\((a, c), b) -> (a, b, c))
    $ lockedZip2S (G.length as) (lockedSwallow2 as cs) (swallow (G.new bs))
  #-}

{-# RULES "lockedStream3/new_3"
    forall as bs cs
    . lockedStream3 as bs (G.new cs)
    = S.map (\((a, b), c) -> (a, b, c))
    $ lockedZip2S (G.length as) (lockedSwallow2 as bs) (swallow (G.new cs))
  #-}



-- Locked Swallowers ---------------------------------------------------------

-- | Swallow two things.
--   There is no end-of vector check.
--   The context needs to know how many elements to demand.
lockedSwallow2 
        :: (Monad m, Vector v a, Vector v b)
        => v a -> v b 
        -> Stream m (a, b)

lockedSwallow2 aa bb
 = aa `seq` bb `seq` n `seq` (S.unfoldr get 0 `S.sized` Unknown)
 where  n        = G.length aa

        {-# INLINE [0] get #-}
        get i   
         | Box a        <- basicUnsafeIndexM aa i
         , Box b        <- basicUnsafeIndexM bb i
         = Just ((a, b), i + 1)
{-# INLINE [1] lockedSwallow2 #-}


-- Locked Stream Zippers -----------------------------------------------------
lockedZip2S 
        :: Monad m 
        => Int
        -> Stream m a -> Stream m b 
        -> Stream m (a, b)

lockedZip2S len
        (Stream mkStep1 sa1 _)
        (Stream mkStep2 sa2 _)
 = Stream step (sa1, sa2, 0) (S.Exact len)
 where 
        {-# INLINE [0] step #-}
        step (s1, s2, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                return $ case (step1, step2) of
                          (Yield x1 s1', Yield x2 s2')
                            | i < len   -> Yield (x1, x2) (s1', s2', i + 1)
                          _             -> Done
{-# INLINE [1] lockedZip2S #-}


lockedZip3S 
        :: Monad m 
        => Int
        -> Stream m a -> Stream m b -> Stream m c
        -> Stream m (a, b, c)

lockedZip3S len
        (Stream mkStep1 sa1 _)
        (Stream mkStep2 sa2 _)
        (Stream mkStep3 sa3 _)
 = Stream step (sa1, sa2, sa3, 0) (S.Exact len)
 where 
        {-# INLINE [0] step #-}
        step (s1, s2, s3, i)
         = i `seq`
           do   step1   <- mkStep1 s1
                step2   <- mkStep2 s2
                step3   <- mkStep3 s3
                return $ case (step1, step2, step3) of
                          (Yield x1 s1', Yield x2 s2', Yield x3 s3')  
                           | i < len    -> Yield (x1, x2, x3) (s1', s2', s3', i + 1)

                          _ -> Done
{-# INLINE [1] lockedZip3S #-}

