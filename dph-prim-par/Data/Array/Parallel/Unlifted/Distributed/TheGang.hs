-- This is a hack which will go away as soon as possible!

module Data.Array.Parallel.Unlifted.Distributed.TheGang (
  setGang, setSequentialGang, theGang
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang 

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

gangRef :: IORef Gang
{-# NOINLINE gangRef #-}
gangRef = unsafePerformIO (newIORef (error "Uninitialised global gang"))

setGang :: Int -> IO ()
setGang n = 
  do
    gang <- forkGang n
    writeIORef gangRef gang

setSequentialGang :: Int -> IO ()
setSequentialGang n = writeIORef gangRef $ sequentialGang n

theGang :: Gang
{-# NOINLINE theGang #-}
theGang = unsafePerformIO (readIORef gangRef)

