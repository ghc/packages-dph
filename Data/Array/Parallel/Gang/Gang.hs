{-# OPTIONS_GHC -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Gang.Gang
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Gang primitives.
--
-- ---------------------------------------------------------------------------

module Data.Array.Parallel.Gang.Gang (
  Gang, forkGang, gangSize, gangIO, gangST
) where

import GHC.Prim                  ( unsafeCoerce# )
import GHC.IOBase
import GHC.ST

import Control.Concurrent        ( forkIO )
import Control.Concurrent.MVar   ( MVar, newEmptyMVar, takeMVar, putMVar )
-- import Control.Monad.ST          ( ST, unsafeIOToST, stToIO )
import Control.Exception         ( assert )
import Control.Monad             ( zipWithM )

-- ---------------------------------------------------------------------------
-- Requests and operations on them

-- | The 'Req' type encapsulates work requests for individual members of a
-- gang. It is made up of an 'IO' action and an 'MVar' which is written to
-- when the action has been executed and can be waited upon.
type Req = (IO (), MVar ())

-- | Create a new request for the given action.
newReq :: IO () -> IO Req
newReq p = do
             mv <- newEmptyMVar
             return (p, mv)

-- | Block until the request has been executed. Note that only one thread can
-- wait for a request.
waitReq :: Req -> IO ()
waitReq = takeMVar . snd

-- | Execute the request and signal its completion.
execReq :: Req -> IO ()
execReq (p, s) = p >> putMVar s ()

-- ---------------------------------------------------------------------------
-- Thread gangs and operations on them

-- | A 'Gang' is a group of threads which execute arbitrary work requests.
data Gang = Gang Int [MVar Req]

-- | The worker thread of a 'Gang'.
gangWorker :: MVar Req -> IO ()
gangWorker mv =
  do
    req <- takeMVar mv
    execReq req
    gangWorker mv

-- | Fork a 'Gang' with the given number of threads.
forkGang :: Int -> IO Gang
forkGang n = do
               mvs <- sequence . replicate n $ newEmptyMVar
               mapM_ (forkIO . gangWorker) mvs
               return $ Gang n mvs

-- | The number of threads in the 'Gang'.
gangSize :: Gang -> Int
gangSize (Gang n _) = n

-- | Issue work requests for the 'Gang' and wait until they have been executed.
-- /There must be exactly one request per 'Gang' member./
gangIO :: Gang -> [IO ()] -> IO ()
gangIO (Gang n mvs) ps =
  assert (length ps == n) $
  do
    reqs <- mapM newReq ps
    zipWithM putMVar mvs reqs
    mapM_ waitReq reqs

-- | Same as 'gangIO' but in the 'ST' monad.
gangST :: Gang -> [ST s ()] -> ST s ()
gangST g = unsafeIOToST . gangIO g . map unsafeSTToIO

-- | Unsafely embed an 'ST' computation in the 'IO' monad without fixing the
-- state type. This should go into 'Control.Monad.ST'.
unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO $ \ s -> (unsafeCoerce# m) s


