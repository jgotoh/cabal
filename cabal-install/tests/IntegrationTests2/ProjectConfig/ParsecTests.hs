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
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Verbosity

-- TODO create tests:
-- - parser tests to read and compare to expected values
-- - golden tests for warnings and errors
parserTests :: [TestTree]
parserTests = [
  -- testCase "read with legacy parser" testLegacyRead
  testCase "read packages" testPackages
  ]

-- Currently I compare the results of legacy parser with the new parser
-- When the parser is implemented I will migrate it to compare to actual values
testLegacyRead :: Assertion
testLegacyRead = do
  httpTransport <- configureTransport verbosity [] Nothing
  projectRootDir <- canonicalizePath basedir

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

testPackages :: Assertion
testPackages = do
  let expected = [".", "packages/packages.cabal"] -- TODO https link, what does legacy parse?
  -- Note that I currently also run the legacy parser to make sure my expected values
  -- do not differ from the non-Parsec implementation, this will be removed in the future
  (config, legacy) <- readConfigDefault "packages"
  assertConfig expected config (projectPackages . condTreeData)
  assertConfig expected legacy (projectPackages . condTreeData)

readConfigDefault :: FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfigDefault rootFp = readConfig rootFp "cabal.project"

readConfig :: FilePath -> FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfig rootFp projectFileName = do
  -- TODO extract argument so it can be mocked
  httpTransport <- configureTransport verbosity [] Nothing
  projectRootDir <- canonicalizePath (basedir </> rootFp)

  let projectRoot = ProjectRootExplicit projectRootDir projectFileName
      extensionName = ""
      distDirLayout = defaultDistDirLayout projectRoot Nothing
      extensionDescription = "description"
      distProjectConfigFp = distProjectFile distDirLayout extensionName
  exists <- doesFileExist distProjectConfigFp
  assertBool ("projectConfig does not exist: " <> distProjectConfigFp) exists
  parsec <- runRebuild projectRootDir $
    readProjectFileSkeleton verbosity httpTransport distDirLayout extensionName extensionDescription
  legacy <- runRebuild projectRootDir $
    readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription
  return (parsec, legacy)

assertConfig :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> IO ()
assertConfig expected config access = expected @=? actual
  where
    actual = access config

-- | Test Utilities
emptyProjectConfig :: ProjectConfig
emptyProjectConfig = mempty

verbosity :: Verbosity
verbosity = minBound --normal --verbose --maxBound --minBound

basedir :: FilePath
basedir = "tests" </> "IntegrationTests2" </> "ProjectConfig" </> "files"
