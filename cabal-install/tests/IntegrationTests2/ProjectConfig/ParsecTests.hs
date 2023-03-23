-- | Integration Tests related to parsing of ProjectConfigs

module IntegrationTests2.ProjectConfig.ParsecTests (parserTests) where

import qualified Data.ByteString       as BS
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options

import Distribution.Client.HttpUtils
import Distribution.Client.DistDirLayout
import Distribution.Client.ProjectConfig
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Verbosity

-- TODO create tests:
-- - parser tests to read and compare to expected values
-- - golden tests for warnings and errors
parserTests :: [TestTree]
parserTests = [
  testCase "read with legacy parser" testLegacyRead
  ]

-- Currently I compare the results of legacy parser with the new parser
-- When the parser is implemented I will migrate it to compare to actual values
testLegacyRead :: Assertion
testLegacyRead = do
  httpTransport <- configureTransport verbosity [] Nothing
  let testdir = "ProjectConfig/files/"
  projectRootDir <- canonicalizePath (basedir </> testdir)

  -- let projectRoot = ProjectRootImplicit projectRootDir
  let projectFileName = "cabal-minimal.project"
      projectRoot = ProjectRootExplicit projectRootDir projectFileName
      extensionName = ""
      distDirLayout = defaultDistDirLayout projectRoot Nothing
      extensionDescription = "description"
      distProjectConfigFp = distProjectFile distDirLayout extensionName
  print distProjectConfigFp
  exists <- doesFileExist distProjectConfigFp
  print $ exists
  projectConfigSkeletonLegacy <- runRebuild projectRootDir $
    readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription
  projectConfigSkeleton <- runRebuild projectRootDir $
    readProjectFileSkeleton verbosity httpTransport distDirLayout extensionName extensionDescription
  projectConfigSkeleton @?= projectConfigSkeletonLegacy

-- | Test Utilities
emptyProjectConfig :: ProjectConfig
emptyProjectConfig = mempty

verbosity :: Verbosity
verbosity = minBound --normal --verbose --maxBound --minBound

basedir :: FilePath
basedir = "tests" </> "IntegrationTests2"
