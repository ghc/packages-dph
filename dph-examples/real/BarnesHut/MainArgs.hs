
module MainArgs
	( MainArg(..)
	, mainArgs)
where
import System.Console.ParseArgs

data MainArg
	= ArgHelp
	| ArgWindowSize
	| ArgDrawTree
	| ArgTimeWarp
	| ArgBodyCount
	| ArgEpsilon
	| ArgDiscSize
	| ArgStartSpeed
	deriving (Eq, Ord, Show)
	
mainArgs :: [Arg MainArg]
mainArgs
 = 	[ Arg	{ argIndex	= ArgHelp
		, argAbbr	= Just 'h'
		, argName	= Just "help"
		, argData	= Nothing
		, argDesc	= "Print this usage help." }

	, Arg	{ argIndex	= ArgWindowSize
		, argAbbr	= Nothing
		, argName	= Just "window"
		, argData	= argDataDefaulted "Int" ArgtypeInt 500
		, argDesc	= "Size of window in pixels (default 500)" }

	, Arg	{ argIndex	= ArgDrawTree
		, argAbbr	= Nothing
		, argName	= Just "tree"
		, argData	= Nothing
		, argDesc	= "Draw the Barnes-Hut quad tree"}

	, Arg	{ argIndex	= ArgTimeWarp
		, argAbbr	= Just 't'
		, argName	= Just "timewarp"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 1
		, argDesc	= "Run the simulation this much faster (default 1)" }

	, Arg	{ argIndex	= ArgBodyCount
		, argAbbr	= Just 'b'
		, argName	= Just "bodies"
		, argData	= argDataDefaulted "Int" ArgtypeInt 100 
		, argDesc	= "Number of bodies in simulation (default 100)" }

	, Arg	{ argIndex	= ArgEpsilon
		, argAbbr	= Just 'e'
		, argName	= Just "epsilon"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 5
		, argDesc	= "Ignore forces between bodies closer than this (default 5)" }
		
	, Arg	{ argIndex	= ArgDiscSize
		, argAbbr	= Just 'd'
		, argName	= Just "disc"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 100
		, argDesc	= "Starting size of disc containing bodies (default 100)" }

	, Arg	{ argIndex	= ArgStartSpeed
		, argAbbr	= Just 's'
		, argName	= Just "speed"
		, argData	= argDataDefaulted "Double" ArgtypeDouble 0.15
		, argDesc	= "Starting speed of bodies (default 0.15)" }
	]
