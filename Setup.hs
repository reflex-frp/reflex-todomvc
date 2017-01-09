{-# LANGUAGE CPP #-}
import Distribution.Simple

#ifndef ghcjs_HOST_OS
import Distribution.MacOSX

guiApps :: [MacApp]
guiApps = [MacApp "reflex-todomvc-wkwebview"
                  Nothing
                  (Just "macos/Info.plist")
                  [] -- No other resources.
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ]
#endif

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
#ifndef ghcjs_HOST_OS
         postBuild = appBundleBuildHook guiApps,
         postCopy = appBundleCopyHook guiApps
#endif
       }

