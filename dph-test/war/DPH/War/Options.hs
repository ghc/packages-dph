
module DPH.War.Options
	( MainArg(..)
	, mainArgs)
where
import System.Console.ParseArgs

data MainArg
	= ArgHelp
	| ArgDebug
	| ArgTestDir
        | ArgJobs               
	| ArgClean
	deriving (Show, Eq, Ord)


mainArgs :: [Arg MainArg]
mainArgs
 =      [ Arg	{ argIndex	= ArgHelp
	        , argAbbr	= Just 'h'
	        , argName	= Just "help"
	        , argData	= Nothing
	        , argDesc	= "Print this usage help." }
        
        , Arg   { argIndex      = ArgDebug
                , argAbbr       = Just 'd'
                , argName       = Just "debug"
                , argData       = Nothing
                , argDesc       = "Emit debugging info for the test driver." }
                
        , Arg   { argIndex      = ArgTestDir
                , argAbbr       = Just 't'
                , argName       = Just "test-dir"
                , argData       = Nothing
                , argDesc       = "Test directories" }
                
        , Arg   { argIndex      = ArgJobs
                , argAbbr       = Just 'j'
                , argName       = Just "jobs"
                , argData       = argDataDefaulted "Int" ArgtypeInt 1
                , argDesc       = "Number of parallel jobs to use" }

        , Arg   { argIndex      = ArgClean
                , argAbbr       = Just 'c'
                , argName       = Just "clean"
                , argData       = Nothing
                , argDesc       = "Cleanup after each test" }
        ]
