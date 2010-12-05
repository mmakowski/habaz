import Distribution.Simple
import Distribution.PackageDescription(PackageDescription)
import Distribution.Simple.LocalBuildInfo(LocalBuildInfo)
import System.Cmd(system)
import Distribution.Simple.LocalBuildInfo
  
main = defaultMainWithHooks (simpleUserHooks {runTests = runAllTests})

runAllTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runAllTests _ _ _ _ = system "echo dupa" >> return ()
