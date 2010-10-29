
module Config
	( Config (..)
	, loadConfig)
where
import MainArgs
import System.Console.ParseArgs

-- | Program config.
data Config
	= Config {
	 
	-- How to present the output.
	  configWindowSize	:: Int
	, configShouldDrawTree	:: Bool
	, configRate		:: Int

	-- What solver to use
	, configSolverName	:: String

	-- System setup
	, configBodyCount	:: Int
	, configBodyMass	:: Double
	, configTimeStep	:: Double
	, configEpsilon		:: Double

	-- Initial conditions.
	, configStartDiscSize	:: Double
	, configStartSpeed	:: Double

	-- Terminating conditions.
	, configMaxSteps	:: Int }
	

-- | Load program config from its command line arguments.	
loadConfig :: Args MainArg -> Config
loadConfig args
 = let	Just windowSize	= getArgInt	args ArgWindowSize
	Just solverName	= getArgString	args ArgSolver
	shouldDrawTree	= gotArg  	args ArgDrawTree
	Just timeStep	= getArgDouble	args ArgTimeStep
	Just rate	= getArgInt	args ArgRate
	Just bodyCount	= getArgInt	args ArgBodyCount
	Just bodyMass	= getArgDouble  args ArgBodyMass
	Just epsilon	= getArgDouble	args ArgEpsilon
	Just discSize	= getArgDouble	args ArgDiscSize
	Just startSpeed	= getArgDouble	args ArgStartSpeed
	Just maxSteps	= getArgInt	args ArgMaxSteps

   in Config
	{ configWindowSize	= windowSize
	, configShouldDrawTree	= shouldDrawTree
	, configRate		= rate
	, configSolverName	= solverName
	, configBodyCount	= bodyCount
	, configBodyMass	= bodyMass
	, configTimeStep	= timeStep
	, configEpsilon		= epsilon
	, configStartDiscSize	= discSize
	, configStartSpeed	= startSpeed
	, configMaxSteps	= maxSteps }