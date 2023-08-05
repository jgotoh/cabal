{-# LANGUAGE RecordWildCards #-}

-- | Integration Tests related to parsing of ProjectConfigs
module IntegrationTests2.ProjectConfig.ParsecTests (parserTests) where

import qualified Data.ByteString as BS
import Data.Either
import Data.Maybe
import Distribution.Client.Dependency.Types (PreSolver (..))
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepoEntry (..), ActiveRepos (..), CombineStrategy (..))
import Distribution.Client.IndexUtils.IndexState (RepoIndexState (..), headTotalIndexState, insertIndexState)
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Parsec
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Client.Targets (readUserConstraint)
import Distribution.Client.Types.AllowNewer (AllowNewer (..), AllowOlder (..), RelaxDepMod (..), RelaxDepScope (..), RelaxDepSubject (..), RelaxDeps (..), RelaxedDep (..))
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Client.Types.SourceRepo
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy (WriteGhcEnvironmentFilesPolicy (..))
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Parsec (simpleParsec)
import Distribution.Simple.Compiler (PackageDB (..))
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs (toPathTemplate)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.Settings (AllowBootLibInstalls (..), CountConflicts (..), FineGrainedConflicts (..), MinimizeConflictSet (..), PreferOldest (..), ReorderGoals (..), StrongFlags (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Types.SourceRepo (KnownRepoType (..), RepoType (..))
import Distribution.Types.Version (mkVersion)
import Distribution.Types.VersionRange.Internal (VersionRange (..))
import Distribution.Utils.NubList
import Distribution.Verbosity
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

-- TODO create tests:
-- - parser tests to read and compare to expected values
-- - golden tests for warnings and errors
parserTests :: [TestTree]
parserTests =
  [ testCase "read packages" testPackages
  , testCase "read optional-packages" testOptionalPackages
  , testCase "read extra-packages" testExtraPackages
  , testCase "read source-repository-package" testSourceRepoList
  , testCase "read project-config-build-only" testProjectConfigBuildOnly
  , testCase "read project-shared" testProjectConfigShared
  ]

testPackages :: Assertion
testPackages = do
  let expected = [".", "packages/packages.cabal"]
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
            { srpType = KnownRepoType Git
            , srpLocation = "https://example.com/Project.git"
            , srpTag = Just "1234"
            , srpBranch = Nothing
            , srpSubdir = []
            , srpCommand = []
            }
        , SourceRepositoryPackage
            { srpType = KnownRepoType Git
            , srpLocation = "https://example.com/example-dir/"
            , srpTag = Just "12345"
            , srpBranch = Nothing
            , srpSubdir = ["subproject"]
            , srpCommand = []
            }
        ]
  (config, legacy) <- readConfigDefault "source-repository-packages"
  assertConfig expected config legacy (projectPackagesRepo . condTreeData)

testExtraPackages :: Assertion
testExtraPackages = do
  let expected =
        [ PackageVersionConstraint (mkPackageName "a") (OrLaterVersion (mkVersion [0]))
        , PackageVersionConstraint (mkPackageName "b") (IntersectVersionRanges (OrLaterVersion (mkVersion [0, 7, 3])) (EarlierVersion (mkVersion [0, 9])))
        ]
  (config, legacy) <- readConfigDefault "extra-packages"
  assertConfig expected config legacy (projectPackagesNamed . condTreeData)

testProjectConfigBuildOnly :: Assertion
testProjectConfigBuildOnly = do
  let expected = ProjectConfigBuildOnly{..}
  (config, legacy) <- readConfigDefault "project-config-build-only"
  assertConfig expected config legacy (projectConfigBuildOnly . condTreeData)
  where
    projectConfigVerbosity = toFlag (toEnum 2)
    projectConfigDryRun = mempty -- cli only
    projectConfigOnlyDeps = mempty -- cli only
    projectConfigOnlyDownload = mempty -- cli only
    projectConfigSummaryFile = toNubList [toPathTemplate "summaryFile"]
    projectConfigLogFile = toFlag $ toPathTemplate "myLog.log"
    projectConfigBuildReports = mempty -- cli only
    projectConfigReportPlanningFailure = toFlag True
    projectConfigSymlinkBinDir = toFlag "some-bindir"
    projectConfigNumJobs = toFlag $ Just 4
    projectConfigKeepGoing = toFlag True
    projectConfigOfflineMode = toFlag True
    projectConfigKeepTempFiles = toFlag True
    projectConfigHttpTransport = toFlag "wget"
    projectConfigIgnoreExpiry = toFlag True
    projectConfigCacheDir = toFlag "some-cache-dir"
    projectConfigLogsDir = toFlag "logs-directory"
    projectConfigClientInstallFlags = mempty -- cli only

testProjectConfigShared :: Assertion
testProjectConfigShared = do
  let rootFp = "project-config-shared"
  projectFileFp <- projectConfigPath rootFp "cabal.project" ""
  let
    projectConfigConstraints = getProjectConfigConstraints projectFileFp
    expected = ProjectConfigShared{..}
  (config, legacy) <- readConfigDefault rootFp
  print (projectConfigShared $ condTreeData legacy)
  assertConfig expected config legacy (projectConfigShared . condTreeData)
  where
    projectConfigDistDir = mempty -- cli only
    projectConfigConfigFile = mempty -- cli only
    projectConfigProjectDir = mempty -- cli only
    projectConfigProjectFile = mempty -- cli only
    projectConfigIgnoreProject = toFlag True
    projectConfigHcFlavor = toFlag GHCJS
    projectConfigHcPath = toFlag "/some/path/to/compiler"
    projectConfigHcPkg = toFlag "/some/path/to/ghc-pkg"
    projectConfigHaddockIndex = toFlag $ toPathTemplate "/path/to/haddock-index"
    projectConfigInstallDirs = mempty -- cli only
    projectConfigPackageDBs = [Nothing, Just (SpecificPackageDB "foo"), Nothing, Just (SpecificPackageDB "bar"), Just (SpecificPackageDB "baz")]
    projectConfigRemoteRepos = mempty -- cli only
    projectConfigLocalNoIndexRepos = mempty -- cli only
    projectConfigActiveRepos = Flag (ActiveRepos [ActiveRepo (RepoName "hackage.haskell.org") CombineStrategyMerge, ActiveRepo (RepoName "my-repository") CombineStrategyOverride])
    projectConfigIndexState =
      let
        hackageState = IndexStateTime $ fromJust $ simpleParsec "2020-05-06T22:33:27Z"
        indexState' = insertIndexState (RepoName "hackage.haskell.org") hackageState headTotalIndexState
        headHackageState = IndexStateTime $ fromJust $ simpleParsec "2020-04-29T04:11:05Z"
        indexState'' = insertIndexState (RepoName "head.hackage") headHackageState headTotalIndexState
       in
        toFlag indexState''
    projectConfigStoreDir = mempty -- cli only
    getProjectConfigConstraints projectFileFp =
      let
        bar = fromRight (error "error parsing bar") $ readUserConstraint "bar == 2.1"
        barFlags = fromRight (error "error parsing bar flags") $ readUserConstraint "bar +foo -baz"
        source = ConstraintSourceProjectConfig projectFileFp
       in
        [(bar, source), (barFlags, source)]
    projectConfigPreferences = [PackageVersionConstraint (mkPackageName "foo") (ThisVersion (mkVersion [0, 9])), PackageVersionConstraint (mkPackageName "baz") (LaterVersion (mkVersion [2, 0]))]
    projectConfigCabalVersion = Flag (mkVersion [1, 24, 0, 1])
    projectConfigSolver = Flag AlwaysModular
    projectConfigAllowOlder = Just (AllowOlder $ RelaxDepsSome [RelaxedDep RelaxDepScopeAll RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "dep")), RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "pkga") (mkVersion [1, 1, 2]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "dep-pkg"))])
    projectConfigAllowNewer = Just (AllowNewer $ RelaxDepsSome [RelaxedDep (RelaxDepScopePackageId (PackageIdentifier (mkPackageName "pkgb") (mkVersion [1, 2, 3]))) RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "dep-pkgb")), RelaxedDep RelaxDepScopeAll RelaxDepModNone (RelaxDepSubjectPkg (mkPackageName "importantlib"))])
    projectConfigWriteGhcEnvironmentFilesPolicy = Flag AlwaysWriteGhcEnvironmentFiles
    projectConfigMaxBackjumps = toFlag 42
    projectConfigReorderGoals = Flag (ReorderGoals True)
    projectConfigCountConflicts = Flag (CountConflicts False)
    projectConfigFineGrainedConflicts = Flag (FineGrainedConflicts False)
    projectConfigMinimizeConflictSet = Flag (MinimizeConflictSet True)
    projectConfigStrongFlags = Flag (StrongFlags True)
    projectConfigAllowBootLibInstalls = Flag (AllowBootLibInstalls True)
    projectConfigOnlyConstrained = mempty -- cli only
    projectConfigPerComponent = mempty -- cli only
    projectConfigIndependentGoals = mempty -- cli only
    projectConfigPreferOldest = Flag (PreferOldest True)
    projectConfigProgPathExtra = mempty
    -- TODO ^ I need to investigate this. The project file of this test says the following: extra-prog-path: /foo/bar, /baz/quux
    -- but the legacy parser always parses an empty list, maybe we have a bug here
    -- this also does not work if using a single path such as extra-prog-path: /foo/bar, list is always empty
    projectConfigMultiRepl = toFlag True

readConfigDefault :: FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfigDefault rootFp = readConfig rootFp "cabal.project"

readConfig :: FilePath -> FilePath -> IO (ProjectConfigSkeleton, ProjectConfigSkeleton)
readConfig rootFp projectFileName = do
  projectRootDir <- canonicalizePath (basedir </> rootFp)

  let projectRoot = ProjectRootExplicit projectRootDir projectFileName
      extensionName = ""
      distDirLayout = defaultDistDirLayout projectRoot Nothing Nothing
      extensionDescription = "description"
  distProjectConfigFp <- projectConfigPath rootFp projectFileName extensionName
  exists <- doesFileExist distProjectConfigFp
  assertBool ("projectConfig does not exist: " <> distProjectConfigFp) exists
  contents <- BS.readFile distProjectConfigFp
  let (_, res) = runParseResult $ parseProjectSkeleton contents
  assertBool ("should parse successfully: " ++ show res) $ isRight res
  let parsec = fromRight undefined res
  httpTransport <- configureTransport verbosity [] Nothing
  legacy <-
    runRebuild projectRootDir $
      readProjectFileSkeletonLegacy verbosity httpTransport distDirLayout extensionName extensionDescription
  return (parsec, legacy)

projectConfigPath :: FilePath -> FilePath -> String -> IO FilePath
projectConfigPath rootFp projectFileName extensionName = do
  projectRootDir <- canonicalizePath (basedir </> rootFp)
  let projectRoot = ProjectRootExplicit projectRootDir projectFileName
      distDirLayout = defaultDistDirLayout projectRoot Nothing Nothing
      distProjectConfigFp = distProjectFile distDirLayout extensionName
  return distProjectConfigFp

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
verbosity :: Verbosity
verbosity = normal -- minBound --normal --verbose --maxBound --minBound

basedir :: FilePath
basedir = "tests" </> "IntegrationTests2" </> "ProjectConfig" </> "files"
