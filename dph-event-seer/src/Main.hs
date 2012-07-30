-- DPH Event Seer.
--      Analyse log files for time spent in GC, time spent with all CPUs active, etc.
--      Compile your program with "-eventlog" and then run with "+RTS -l",
--      then parse the eventlog with this.
module Main where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import System.Environment
import System.IO
import System.Exit

import HecUsage
import HecWake

main :: IO ()
main = getArgs >>= command

command :: [String] -> IO ()

command ["--help"] = do
    putStr usage

command ["show", file] = do
    eventLog <- readLogOrDie file
    putStrLn $ ppEventLog eventLog

command ["usage", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate hecUsageMachine capEvents
    putStrLn $ showValidate showHecUsage show result

command ["wake", file] = do
    eventLog <- readLogOrDie file
    let capEvents = sortEvents . events . dat $ eventLog
    let result = validate hecWakeMachine capEvents
    putStrLn $ showValidate showHecWake show result

command _ = putStr usage >> die "Unrecognized command"

usage :: String
usage = unlines $ map pad strings
 where
    align = 4 + (maximum . map (length . fst) $ strings)
    pad (x, y) = zipWith const (x ++ repeat ' ') (replicate align ()) ++ y
    strings = [ ("dph-event-seer --help:",                     "Display this help.")

              , ("dph-event-seer show <file>:",                "Raw event log data.")

              , ("dph-event-seer usage <file>:",        "Show amount of time spent with n HECs active, and in GC")
              , ("dph-event-seer wake <file>:",        "Times between waking a thread on a different HEC and it starting")
              , ("", "")
              , ("", "Compile your program with '-eventlog' then run with '+RTS -l' to produce a .eventlog.")
              , ("", "Then analyse the output with this program.")
              ]

readLogOrDie :: FilePath -> IO EventLog
readLogOrDie file = do
    e <- readEventLogFromFile file
    case e of
        Left s    -> die ("Failed to parse " ++ file ++ ": " ++ s)
        Right evlog -> return evlog

die :: String -> IO a
die s = do hPutStrLn stderr s; exitWith (ExitFailure 1)
