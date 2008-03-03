module Testsuite.Testcase (
  Test(..), mkTest, runTests
) where

import Test.QuickCheck
import Test.QuickCheck.Batch (TestResult(..), run, defOpt)

import Text.Regex.Base

import System.Environment (getArgs)

import Data.Maybe (isJust)

import IO

data Test = Test { testName     :: String
                 , testProperty :: Property
                 }

mkTest :: Testable a => String -> a -> Test
mkTest name = Test name . property

runTests :: [Test] -> IO ()
runTests tests =
  do
    args <- getArgs
    mapM_ chk $ pick args tests
  where
    chk (Test { testName = name, testProperty = prop }) =
      do
        putStr $ name ++ spaces (60 - length name) ++ "... "
        hFlush stdout
        res <- run prop defOpt
        case res of
          TestOk       _ n _ -> putStrLn $ "pass (" ++ show n ++ ")"
          TestExausted _ n _ -> putStrLn $ "EXHAUSTED (" ++ show n ++ ")"
          TestFailed   s n   ->
            do
              putStrLn $ "FAIL (" ++ show n ++ ")"
              mapM_ putStrLn $ map ("    " ++) s
          TestAborted   e     ->
            do
              putStrLn $ "ABORTED"
              putStrLn $ "    " ++ show e
        hFlush stdout
    spaces n | n <= 0    = ""
             | otherwise = replicate n ' '

pick :: [String] -> [Test] -> [Test]
pick [] = id
pick ss = filter (match (map mkRegex ss))
  where
    match :: [Regex] -> Test -> Bool
    match rs tst = any (\r -> isJust . matchRegex r $ testName tst) rs

