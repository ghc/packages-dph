{-# LANGUAGE PatternGuards #-}
module BuildTest
	( BuildResults(..)
	, buildTest)
where
import Benchmarks
import Config
import BuildBox
import Data.Maybe
import Control.Monad


-- | Run regression tests.	
buildTest :: Config -> Environment -> Build ()
buildTest config env
 = do	outLn "* Running regression tests"
	
	-- Get the current time.
	utcTime	<- io $ getCurrentTime

	-- Load the baseline file if it was given.
	mBaseline <- case configAgainstResults config of
			Nothing		-> return Nothing
			Just fileName
			 -> do	file	<- io $ readFile fileName
				return	$ Just file
				
	let resultsPrior
		= maybe []
			(\contents -> map statBenchResult $ buildResultBench $ read contents)
			mBaseline
	let scratchDir 
		= fromMaybe ("buildTest: must specify --scratch") 
		$ configScratchDir config

	-- Run the DPH benchmarks
	benchResultsDPH
	 <- if (configDoTestDPH config)
	     then inDir (scratchDir ++ "/ghc-head/libraries/dph") $
 	  	  do	mapM 	(outRunBenchmarkWith (configIterations config)  resultsPrior)
				(benchmarksDPH config)
	     else return []

	-- Run the Repa benchmarks
	benchResultsRepa
	 <- if (configDoTestRepa config)
	     then inDir (scratchDir ++ "/repa-head") $
 	 	  do	mapM 	(outRunBenchmarkWith (configIterations config)  resultsPrior)
				(benchmarksRepa config)
	     else return []

	-- Run NoSlow benchmarks
	benchResultsNoSlow
	 <- if (configDoTestNoSlow config)
	     then 
		inDir (scratchDir ++ "/NoSlow-head") $
		withTempFile $ \filePath ->
		 do	ssystem  $ "dist/build/noslow/noslow -o " ++ filePath
			liftM parseNoSlowLog $ io $ readFile filePath
		
	     else return []
	

	let benchResults
		= benchResultsDPH ++ benchResultsRepa ++ benchResultsNoSlow

	-- Make the build results.
	let buildResults
		= BuildResults
		{ buildResultTime		= utcTime
		, buildResultEnvironment	= env
		, buildResultBench		= benchResults }

	-- Write results to a file if requested.	
	maybe 	(return ())
		(\(fileName, shouldStamp) -> do
			stamp	<- if shouldStamp
				 	then io $ getStampyTime
					else return ""
					
			
			let fileName'	= fileName ++ stamp
			
			outLn $ "* Writing results to " ++ fileName'
			io $ writeFile fileName' $ show buildResults)
		(configWriteResults config)
	

	-- Compute comparisons against the baseline file.
	let resultComparisons
	 	= compareManyBenchResults 
			(resultsPrior)
			(map statBenchResult benchResults)

	
	-- Mail results to recipient if requested.
	let spaceHack = text . unlines . map (\l -> " " ++ l) . lines
	maybe 	(return ())
		(\(from, to) -> do
			outLn $ "* Mailing results to " ++ to 
			mail	<- createMailWithCurrentTime from to "[nightly] DPH Performance Test Succeeded"
				$ render $ vcat
				[ text "DPH Performance Test Succeeded"
				, blank
				, ppr env
				, blank
				, spaceHack $ render $ reportBenchResults (configSwingFraction config) resultComparisons
				, blank ]
			
			outLn $ "  - Writing mail file"
			io $ writeFile "dph-buildbot.mail" $ render $ renderMail mail
				
			outLn $ "  - Sending mail"
			sendMailWithMailer mail defaultMailer				
			return ())
		(configMailFromTo config)


-- | Parse a noslow benchmark results files.
parseNoSlowLog :: String -> [BenchResult Single]
parseNoSlowLog str
	= map parseNoSlowLine
	$ tail
	$ lines str

parseNoSlowLine :: String -> BenchResult Single
parseNoSlowLine str
 = let	[name, mean, meanLB, meanUB, _, _, _]
		= words $ map (\c -> if c == ',' then ' ' else c) str
	
   in	BenchResult
		{ benchResultName	= normaliseNoSlowName $ read name
		, benchResultRuns	
			= [ BenchRunResult 0
				[ Time KernelWall `secs` (read meanLB) 
				, Time KernelWall `secs` (read mean)
				, Time KernelWall `secs` (read meanUB) ]] }


-- | Normalise the name of a noslow benchmark to match our format.
normaliseNoSlowName :: String -> String
normaliseNoSlowName name
	= ("noslow." ++)
	$ map (\c -> if c == '/' then '.' else c)
	$ name
