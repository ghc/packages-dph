module Testsuite.Testcase (
  Test(..), mkTest, runTests
) where

import Test.QuickCheck
import Test.QuickCheck.Batch (TestResult(..), run, defOpt)

data Test = Test { testName     :: String
                 , testProperty :: Property
                 }

mkTest :: Testable a => String -> a -> Test
mkTest name = Test name . property

runTests :: [Test] -> IO ()
runTests = mapM_ chk
  where
    chk (Test { testName = name, testProperty = prop }) =
      do
        putStr $ name ++ spaces (60 - length name) ++ "... "
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
    spaces n | n <= 0    = ""
             | otherwise = replicate n ' '

