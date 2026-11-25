{-# language BlockArguments #-}

-- | Runs ch-hs-imports on files in small projects under test/golden and
-- compares what it prints for each with the @.golden@ file beside it. Run
-- with @--accept@ to update the golden files.
--
-- Each project has the @ghc-pkg dump@ output to use in its @ghc-pkg-dump@
-- file, which a stand-in @ghc-pkg@ prints, so the results do not depend on
-- the packages installed where the tests run.
module Main (main) where

-- base
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))

-- bytestring
import qualified Data.ByteString.Lazy as LazyByteString

-- directory
import System.Directory (Permissions (..), getPermissions, setPermissions)

-- filepath
import System.FilePath ((<.>), (</>))

-- process
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)

-- tasty-golden
import Test.Tasty.Golden (goldenVsString)

-- temporary
import System.IO.Temp (withSystemTempDirectory)

-- text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


main :: IO ()
main =
  withSystemTempDirectory "ch-hs-imports-golden" \bin -> do
    let ghcPkg = bin </> "ghc-pkg"
    writeFile ghcPkg "#!/bin/sh\nexec cat ghc-pkg-dump\n"
    permissions <- getPermissions ghcPkg
    setPermissions ghcPkg permissions {executable = True}

    defaultMain $
      testGroup
        "ch-hs-imports"
        [ -- Another package's folder name is a prefix of the file's package's.
          golden bin "sibling-folder" "foo-bar/src/FooBar.hs"
        , -- The file's package is nested in another package's folder.
          golden bin "nested-package" "outer/inner/src/Inner.hs"
        , -- The file is in a test suite whose dependencies, from a common
          -- stanza, follow the library's in the .cabal file.
          golden bin "component-dependencies" "pkg/test/Main.hs"
        , -- Two packages define the imported module, and one's folder name is
          -- a prefix of the other's.
          golden bin "duplicate-local-module" "app/src/App.hs"
        , -- The file depends on a package and on another that re-exports its
          -- module.
          golden bin "re-export" "both/src/Both.hs"
        , -- The file depends only on the package that re-exports the module.
          golden bin "re-export" "only/src/Only.hs"
        ]


-- | A test that ch-hs-imports prints the golden file for the file at the
-- given path in the given project, with the stand-in ghc-pkg in @bin@.
golden :: FilePath -> FilePath -> FilePath -> TestTree
golden bin project file =
  goldenVsString
    (project </> file)
    ("test/golden" </> project </> file <.> "golden")
    do
      environment <- getEnvironment
      let path = bin ++ maybe "" (':' :) (lookup "PATH" environment)
      (exitCode, out, err) <-
        readCreateProcessWithExitCode
          (proc "ch-hs-imports" ["--local-modules-from-current-dir", file])
            { cwd = Just ("test/golden" </> project)
            , env = Just (("PATH", path) : filter ((/= "PATH") . fst) environment)
            }
          ""
      pure . LazyByteString.fromStrict . Text.encodeUtf8 . Text.pack . concat $
        [ out
        , if null err then "" else "\n-- stderr:\n" ++ err
        , case exitCode of
            ExitSuccess -> ""
            ExitFailure code -> "\n-- exit code: " ++ show code ++ "\n"
        ]
