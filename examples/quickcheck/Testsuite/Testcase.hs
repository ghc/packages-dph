module Testsuite.Testcase (
  Test(..), mkTest, runTests
) where

import Test.QuickCheck
--import Test.QuickCheck.Batch (TestResult(..), run, defOpt)

import Text.Regex

import System.Environment (getArgs)

import Data.Maybe (isJust)

import System.IO

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
        res <- quickCheckWithResult customArgs prop
        case res of
          Success n _ _ -> putStrLn $ "pass (" ++ show n ++ ")"
          GaveUp  n _ _ -> putStrLn $ "EXHAUSTED (" ++ show n ++ ")"
          Failure n _ _ _ _ _ s -> do
                          putStrLn $ "FAILED (" ++ show n ++ ")"
                          putStrLn $ indent s
          NoExpectedFailure
                  n _ _ -> putStrLn $ "NO EXPECTED FAILURE (" ++ show n ++ ")"
        hFlush stdout
    spaces n | n <= 0    = ""
             | otherwise = replicate n ' '
    customArgs = stdArgs { chatty = False } -- do not print to stdout
    indent = unlines . map (spaces 4 ++) . lines 

pick :: [String] -> [Test] -> [Test]
pick [] = id
pick ss = filter (match (map mkRegex ss))
  where
    match :: [Regex] -> Test -> Bool
    match rs tst = any (\r -> isJust . matchRegex r $ testName tst) rs

