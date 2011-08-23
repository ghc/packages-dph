
module DPH.War.Job.Compile
	(jobCompile)
where
import DPH.War.Result
import DPH.War.Job
import BuildBox
import System.FilePath
import System.Directory
import Control.Monad
import Data.List


-- | Compile a Haskell Source File
jobCompile :: Job -> Build [Result]
jobCompile (JobCompile
		testName _wayName srcHS optionsGHC
		buildDir mainCompOut mainCompErr
		mainBin)

 = do	needs srcHS
	
	-- The directory holding the Main.hs file.
	let (srcDir, srcFile)	= splitFileName srcHS
		
	-- Copy the .hs files to the build directory.
	-- This freshens them and ensures we won't conflict with other make jobs
	-- running on the same source files, but in different ways.
	ensureDir buildDir
	sources	<- io
		$  liftM (filter (\f -> isSuffixOf ".hs" f))
		$  lsFilesIn srcDir

	qssystem $ "cp " ++ (intercalate " " sources) ++ " " ++ buildDir

	-- The copied version of the root source file.
	let srcCopyHS	= buildDir </> srcFile
	
	(time, (code, strOut, strErr))
	  <- runTimedCommand
	  $  systemTee False
		("ghc " ++ " -XTemplateHaskell"
		        ++ " -iwar"
		        ++ " -iwar/DPH"
		        ++ " -package dph-prim-seq"
		        ++ " -outputdir " ++ buildDir 
		        ++ " --make "     ++ srcCopyHS
		        ++ " -o "         ++ mainBin)
		""

	atomicWriteFile mainCompOut strOut
	atomicWriteFile mainCompErr strErr        

        let success     = case code of
                                ExitFailure _   -> False
                                _               -> True
                                
        when (not success)
         $ do   io $ putStrLn strErr
                io $ putStrLn strOut

	let ftime	= fromRational $ toRational time
	return  $  [ ResultAspect $ Time TotalWall `secs` ftime]
	        ++ (if success then [] else [ResultUnexpectedFailure])

