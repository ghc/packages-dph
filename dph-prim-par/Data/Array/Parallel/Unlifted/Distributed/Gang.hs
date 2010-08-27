{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Distributed.Gang
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Gang primitives.
--
-- /TODO:/
--
-- * Implement busy waiting.
--
-- * Benchmark.
--
-- * Generalise thread indices?

-- #define SEQ_IF_GANG_BUSY 1
-- #define TRACE_GANG 1

module Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, seqGang, forkGang, gangSize, gangIO, gangST, traceGang, traceGangST 
) where

import GHC.IOBase
import GHC.ST
import GHC.Conc                  ( forkOnIO )
import GHC.Exts                  ( traceEvent )

import Control.Concurrent.MVar
import Control.Exception         ( assert )
import Control.Monad             ( zipWithM, zipWithM_ )

import System.Time ( ClockTime(..), getClockTime )

-- ---------------------------------------------------------------------------
-- Requests and operations on them

-- | The 'Req' type encapsulates work requests for individual members of a
--   gang. It is made up of an 'IO' action, parametrised by the index of the
--   worker which executes it, and an 'MVar' which is written to when the action
--   has been executed and can be waited upon.
type Req = (Int -> IO (), MVar ())


-- | Create a new request for the given action.
newReq :: (Int -> IO ()) -> IO Req
newReq p = do
             mv <- newEmptyMVar
             return (p, mv)


-- | Block until the request has been executed. Note that only one thread can
--   wait for a request.
waitReq :: Req -> IO ()
waitReq = takeMVar . snd


-- | Execute the request and signal its completion.
execReq :: Int -> Req -> IO ()
execReq i (p, s) = p i >> putMVar s ()


-- ---------------------------------------------------------------------------
-- Thread gangs and operations on them

-- | A 'Gang' is a group of threads which execute arbitrary work
--   requests. To get the gang to do work, write Req-uest values
--   to its MVars

data Gang = Gang !Int           -- Number of 'Gang' threads
                 [MVar Req]     -- One 'MVar' per thread
                 (MVar Bool)    -- Indicates whether the 'Gang' is busy


instance Show Gang where
  showsPrec p (Gang n _ _) = showString "<<"
                           . showsPrec p n
                           . showString " threads>>"


-- | A sequential gang has no threads.
seqGang :: Gang -> Gang
seqGang (Gang n _ mv) = Gang n [] mv


-- | The worker thread of a 'Gang'.
gangWorker :: Int -> MVar Req -> IO ()
gangWorker i mv =
  do
    req 	<- takeMVar mv
    traceGang ("Worker " ++ show i ++ " begin")

    start 	<- getGangTime
    execReq i req
    end 	<- getGangTime

    traceGang ("Worker " ++ show i ++ " end (" ++ diffTime start end ++ ")")
    gangWorker i mv


-- | Fork a 'Gang' with the given number of threads (at least 1).
forkGang :: Int -> IO Gang
forkGang n = assert (n > 0) $
             do
               mvs <- sequence . replicate n $ newEmptyMVar
               zipWithM_ forkOnIO [0..] (zipWith gangWorker [0 .. n-1] mvs)
               busy <- newMVar False
               return $ Gang n mvs busy


-- | The number of threads in the 'Gang'.
gangSize :: Gang -> Int
gangSize (Gang n _ _) = n


-- | Issue work requests for the 'Gang' and wait until they have been executed.
gangIO	:: Gang
	-> (Int -> IO ())
	-> IO ()

#if SEQ_IF_GANG_BUSY
gangIO (Gang n mvs busy) p =
  do
    traceGang   "gangIO: issuing work requests (SEQ_IF_GANG_BUSY)"
    b <- swapMVar busy True

    traceGang $ "gangIO: gang is currently " ++ (if b then "busy" else "idle")

    if b
      then mapM_ p [0 .. n-1]
      else do
             parIO n mvs p
             swapMVar busy False
             return ()
#else
gangIO (Gang n [] busy)  p = mapM_ p [0 .. n-1]
gangIO (Gang n mvs busy) p = parIO n mvs p
#endif


parIO :: Int -> [MVar Req] -> (Int -> IO ()) -> IO ()
parIO n mvs p =
  do
    traceGang "parIO begin"

    start 	<- getGangTime
    reqs 	<- sequence . replicate n $ newReq p
    zipWithM putMVar mvs reqs
    mapM_ waitReq reqs
    end 	<- getGangTime

    traceGang $ "parIO end " ++ diffTime start end


-- | Same as 'gangIO' but in the 'ST' monad.
gangST :: Gang -> (Int -> ST s ()) -> ST s ()
gangST g p = unsafeIOToST . gangIO g $ unsafeSTToIO . p


-- Tracing -------------------------------------------------------------------
#if TRACE_GANG
getGangTime :: IO Integer
getGangTime = do
                TOD sec pico <- getClockTime
                return (pico + sec * 1000000000000)

diffTime :: Integer -> Integer -> String
diffTime x y = show (y-x)

traceGang :: String -> IO ()
traceGang s = do
                t <- getGangTime
                traceEvent $ show t ++ " @ " ++ s

#else
getGangTime :: IO ()
getGangTime = return ()

diffTime :: () -> () -> String
diffTime _ _ = ""

traceGang :: String -> IO ()
traceGang _ = return ()

#endif


traceGangST :: String -> ST s ()
traceGangST s = unsafeIOToST (traceGang s)


{- Comes from GHC.IOBase now...
-- | Unsafely embed an 'ST' computation in the 'IO' monad without fixing the
-- state type. This should go into 'Control.Monad.ST'.
unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO $ \ s -> (unsafeCoerce# m) s
 -}


{- Old code for sequential gang
-- gangIO (SeqGang n )      p = mapM_ p [0 .. n-1]
-- gangSize (SeqGang n)  = n


A /sequential/ 'Gang' simulates such a group by executing work
-- requests sequentially.


-- | Yield a sequential 'Gang' which simulates the given number of threads.
sequentialGang :: Int -> Gang
sequentialGang n = assert (n > 0) $ SeqGang n

-- | Yield a sequential 'Gang' which simulates the given one.
seqGang :: Gang -> Gang
seqGang = sequentialGang . gangSize
-}


