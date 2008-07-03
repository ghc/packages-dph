import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo

import System.IO
import System.Environment

main :: IO ()
main = do args <- getArgs
          let argss = case args of
                      "configure" : args' ->
                          ["configure" :
                           "--distpref=dist-seq" :
                           "--flags=seq" :
                           args',
                           "configure" :
                           "--distpref=dist-par" :
                           "--flags=par" :
                           args']
                      cmd : args'
                       | cmd `elem` ["build", "copy", "install",
                                     "register", "unregister", "clean"] ->
                          [cmd : "--distpref=dist-seq" : args',
                           cmd : "--distpref=dist-par" : args']
                      _ -> [args]
          mapM_ realMain argss
          case args of
            "configure" : _ ->
              do
                hackPackageName "dist-seq" "dph-seq"
                hackPackageName "dist-par" "dph-par"
            _ -> return ()

realMain :: [String] -> IO ()
realMain args = withArgs args
              $ defaultMainWithHooks
              $ simpleUserHooks {
                    haddockHook = \_ _ _ _ -> warn
                }
  where
    warn = hPutStrLn stderr "WARNING: ndp not supported by hadock"

hackPackageName :: FilePath -> String -> IO ()
hackPackageName distPref name = do
    lbi <- getPersistBuildConfig distPref
    let lpd = localPkgDescr lbi
        pkg = package lpd
        pkg' = pkg { pkgName = PackageName name }
        lpd' = lpd { package = pkg' }
        lbi' = lbi { localPkgDescr = lpd' }
    writePersistBuildConfig distPref lbi'

