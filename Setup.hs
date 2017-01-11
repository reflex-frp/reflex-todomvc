{-# LANGUAGE CPP #-}
import Distribution.Simple

#if !defined(ghcjs_HOST_OS) && defined(MIN_VERSION_cabal_macosx)
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
main = defaultMainWithHooks simpleUserHooks
#if !defined(ghcjs_HOST_OS) && defined(MIN_VERSION_cabal_macosx)
       { postBuild = appBundleBuildHook guiApps
       , postCopy = appBundleCopyHook guiApps
       }
#endif

