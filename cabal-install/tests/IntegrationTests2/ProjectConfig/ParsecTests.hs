-- | Integration Tests related to parsing of ProjectConfigs
module IntegrationTests2.ProjectConfig.ParsecTests (parserTests) where

import qualified Data.ByteString as BS
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Parsec
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.Types.SourceRepo
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Types.SourceRepo (KnownRepoType (..), RepoType (..))
import Distribution.Types.Version (Version, mkVersion)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options

-- TODO create tests:
-- - parser tests to read and compare to expected values
-- - golden tests for warnings and errors
parserTests :: [TestTree]
parserTests =
  [ testCase "read packages" testPackages,
    testCase "read optional-packages" testOptionalPackages,
    testCase "read extra-packages" testExtraPackages,
    testCase "read source-repository-package" testSourceRepoList
  ]

testPackages :: Assertion
testPackages = do
  let expected = [".", "packages/packages.cabal"] -- TODO also test https link
  -- Note that I currently also run the legacy parser to make sure my expected values
  -- do not differ from the non-Parsec implementation, this will be removed in the future
  (config, legacy) <- readConfigDefault "packages"
  assertConfig expected config legacy (projectPackages . condTreeData)

testOptionalPackages :: Assertion
testOptionalPackages = do
  let expected = [".", "packages/packages.cabal"]
  (config, legacy) <- readConfigDefault "optional-packages"
  assertConfig expected config legacy (projectPackagesOptional . condTreeData)

testSourceRepoList :: Assertion
testSourceRepoList = do
  let expected =
        [ SourceRepositoryPackage
            { srpType = KnownRepoType Git,
              srpLocation = "https://example.com/Project.git",
              srpTag = Just "1234",
              srpBranch = Nothing,
              srpSubdir = [],
              srpCommand = []
            },
          SourceRepositoryPackage
            { srpType = KnownRepoType Git,
              srpLocation = "https://example.com/example-dir/",
              srpTag = Just "12345",
              srpBranch = Nothing,
              srpSubdir = ["subproject"],
              srpCommand = []
            }
        ]
  (config, legacy) <- readConfigDefault "source-repository-packages"
  assertConfig expected config legacy (projectPackagesRepo . condTreeData)

testExtraPackages :: Assertion
testExtraPackages = do
  let expected =
        [ PackageVersionConstraint (mkPackageName "a") (OrLaterVersion (mkVersion [0])),
          PackageVersionConstraint (mkPackageName "b") (IntersectVersionRanges (OrLaterVersion (mkVersion [0, 7, 3])) (EarlierVersion (mkVersion [0, 9])))
        ]
  (config, legacy) <- readConfigDefault "extra-packages"
  assertConfig expected config legacy (projectPackagesNamed . condTreeData)

readConfigDefault :: FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfigDefault rootFp = readConfig rootFp "cabal.project"

-- TODO this is an overkill, look at warningTests, they just use runParseResult without
-- httpTransport etc
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
  parsec <-
    runRebuild projectRootDir $
      readProjectFileSkeleton verbosity httpTransport distDirLayout extensionName extensionDescription
  legacy <-
    runRebuild projectRootDir $
      readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription
  return (parsec, legacy)

assertConfig' :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> IO ()
assertConfig' expected config access = expected @=? actual
  where
    actual = access config

assertConfig :: (Eq a, Show a) => a -> ProjectConfigSkeleton -> ProjectConfigSkeleton -> (ProjectConfigSkeleton -> a) -> IO ()
assertConfig expected config configLegacy access = do
  expected @=? actualLegacy
  expected @=? actual
  where
    actual = access config
    actualLegacy = access configLegacy

-- | Test Utilities
emptyProjectConfig :: ProjectConfig
emptyProjectConfig = mempty

verbosity :: Verbosity
verbosity = normal -- minBound --normal --verbose --maxBound --minBound

basedir :: FilePath
basedir = "tests" </> "IntegrationTests2" </> "ProjectConfig" </> "files"
