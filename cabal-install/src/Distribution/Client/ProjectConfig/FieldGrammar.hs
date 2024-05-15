{-# LANGUAGE OverloadedStrings #-}

-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar
  ( projectConfigFieldGrammar
  , packageConfigFieldGrammar
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import Distribution.CabalSpecVersion (CabalSpecVersion (..))
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (PackageConfig (..), ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigProvenance (..), ProjectConfigShared (..))
import Distribution.Client.Utils.Parsec
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.Simple.Flag
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))

-- TODO check usages of monoidalField: "Field which can be define multiple times, and the results are mappended."
-- I've used it often for fields that should not be appended if defined multiple times, basically any field that is not a list
-- I expect I can just use optionalFieldDef/Ala in these cases, see optionalFieldDefAla "haddock-css"
-- TODO check if ^^^ availableSince can be used in some of the fields (see FieldGrammar of PackageDescription)

-- TODO ParsecFieldGrammar' is a Grammar implementation, we should just use abstract FieldGrammar here
projectConfigFieldGrammar :: ProjectConfigPath -> [String] -> ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar source knownPrograms =
  ProjectConfig
    <$> monoidalFieldAla "packages" (alaList' FSep Token) L.projectPackages
    <*> monoidalFieldAla "optional-packages" (alaList' FSep Token) L.projectPackagesOptional
    <*> pure mempty -- source-repository-package stanza
    <*> monoidalFieldAla "extra-packages" formatPackageVersionConstraints L.projectPackagesNamed
    <*> blurFieldGrammar L.projectConfigBuildOnly projectConfigBuildOnlyFieldGrammar
    <*> blurFieldGrammar L.projectConfigShared (projectConfigSharedFieldGrammar source)
    <*> pure provenance
    <*> pure mempty
    -- \^ PackageConfig to be applied to all packages, specified inside 'package *' stanza
    <*> blurFieldGrammar L.projectConfigLocalPackages (packageConfigFieldGrammar knownPrograms)
    -- \^ PackageConfig to be applied to locally built packages, specified not inside a stanza
    <*> pure mempty
  where
    -- \^ PackageConfig applied to explicitly named packages
    provenance = Set.singleton (Explicit source)

formatPackageVersionConstraints :: [PackageVersionConstraint] -> List CommaVCat (Identity PackageVersionConstraint) PackageVersionConstraint
formatPackageVersionConstraints = alaList CommaVCat

projectConfigBuildOnlyFieldGrammar :: ParsecFieldGrammar' ProjectConfigBuildOnly
projectConfigBuildOnlyFieldGrammar =
  ProjectConfigBuildOnly
    <$> optionalFieldDef "verbose" L.projectConfigVerbosity mempty
    <*> pure mempty -- cli flag: projectConfigDryRun
    <*> pure mempty -- cli flag: projectConfigOnlyDeps
    <*> pure mempty -- cli flag: projectConfigOnlyDownload
    <*> monoidalFieldAla "build-summary" (alaNubList VCat) L.projectConfigSummaryFile
    <*> optionalFieldDef "build-log" L.projectConfigLogFile mempty
    <*> optionalFieldDef "remote-build-reporting" L.projectConfigBuildReports mempty
    <*> optionalFieldDef "report-planning-failure" L.projectConfigReportPlanningFailure mempty
    <*> monoidalFieldAla "symlink-bindir" (alaFlag FilePathNT) L.projectConfigSymlinkBinDir
    <*> monoidalFieldAla "jobs" (alaFlag NumJobs) L.projectConfigNumJobs
    <*> optionalFieldDef "semaphore" L.projectConfigUseSemaphore mempty
    <*> optionalFieldDef "keep-going" L.projectConfigKeepGoing mempty
    <*> optionalFieldDef "offline" L.projectConfigOfflineMode mempty
    <*> optionalFieldDef "haddock-keep-temp-files" L.projectConfigKeepTempFiles mempty
    <*> monoidalFieldAla "http-transport" (alaFlag Token) L.projectConfigHttpTransport
    <*> optionalFieldDef "ignore-expiry" L.projectConfigIgnoreExpiry mempty
    <*> monoidalFieldAla "remote-repo-cache" (alaFlag FilePathNT) L.projectConfigCacheDir
    <*> monoidalFieldAla "logs-dir" (alaFlag FilePathNT) L.projectConfigLogsDir
    <*> pure mempty

projectConfigSharedFieldGrammar :: ProjectConfigPath -> ParsecFieldGrammar' ProjectConfigShared
projectConfigSharedFieldGrammar source =
  ProjectConfigShared
    <$> optionalFieldDefAla "builddir" (alaFlag FilePathNT) L.projectConfigDistDir mempty -- TODO builddir is not documented
    <*> pure mempty -- cli flag: projectConfigConfigFile
    <*> optionalFieldDefAla "project-dir" (alaFlag FilePathNT) L.projectConfigProjectDir mempty
    <*> optionalFieldDefAla "project-file" (alaFlag FilePathNT) L.projectConfigProjectFile mempty
    <*> optionalFieldDef "ignore-project" L.projectConfigIgnoreProject mempty
    <*> optionalFieldDef "compiler" L.projectConfigHcFlavor mempty
    <*> monoidalFieldAla "with-compiler" (alaFlag FilePathNT) L.projectConfigHcPath
    <*> monoidalFieldAla "with-hc-pkg" (alaFlag FilePathNT) L.projectConfigHcPkg
    <*> optionalFieldDef "doc-index-file" L.projectConfigHaddockIndex mempty
    <*> pure mempty -- cli flag: projectConfigInstallDirs
    <*> monoidalFieldAla "package-dbs" (alaList' CommaFSep PackageDBNT) L.projectConfigPackageDBs
    <*> pure mempty -- cli flag: projectConfigRemoteRepos
    <*> pure mempty -- cli flag: projectConfigLocalNoIndexRepos
    <*> monoidalField "active-repositories" L.projectConfigActiveRepos
    <*> monoidalField "index-state" L.projectConfigIndexState
    <*> optionalFieldDefAla "store-dir" (alaFlag FilePathNT) L.projectConfigStoreDir mempty
    <*> monoidalFieldAla "constraints" (alaList' FSep ProjectConstraints) L.projectConfigConstraints
      ^^^ (fmap . fmap) (\(userConstraint, _) -> (userConstraint, ConstraintSourceProjectConfig source))
    <*> monoidalFieldAla "preferences" formatPackageVersionConstraints L.projectConfigPreferences
    <*> monoidalField "cabal-lib-version" L.projectConfigCabalVersion
    <*> monoidalField "solver" L.projectConfigSolver
    <*> optionalField "allow-older" L.projectConfigAllowOlder
    <*> optionalField "allow-newer" L.projectConfigAllowNewer
    <*> monoidalField "write-ghc-environment-files" L.projectConfigWriteGhcEnvironmentFilesPolicy
    <*> monoidalFieldAla "max-backjumps" (alaFlag MaxBackjumps) L.projectConfigMaxBackjumps
    <*> monoidalField "reorder-goals" L.projectConfigReorderGoals
    <*> monoidalField "count-conflicts" L.projectConfigCountConflicts
    <*> monoidalField "fine-grained-conflicts" L.projectConfigFineGrainedConflicts
    <*> monoidalField "minimize-conflict-set" L.projectConfigMinimizeConflictSet
    <*> monoidalField "strong-flags" L.projectConfigStrongFlags
    <*> monoidalField "allow-boot-library-installs" L.projectConfigAllowBootLibInstalls
    <*> optionalFieldDef "reject-unconstrained-dependencies" L.projectConfigOnlyConstrained mempty
    <*> optionalFieldDef "per-component" L.projectConfigPerComponent mempty
    <*> optionalFieldDef "independent-goals" L.projectConfigIndependentGoals mempty
    <*> monoidalField "prefer-oldest" L.projectConfigPreferOldest
    <*> monoidalFieldAla "extra-prog-path-shared-only" (alaNubList' FSep FilePathNT) L.projectConfigProgPathExtra
    <*> monoidalField "multi-repl" L.projectConfigMultiRepl

packageConfigFieldGrammar :: [String] -> ParsecFieldGrammar' PackageConfig
packageConfigFieldGrammar knownPrograms =
  PackageConfig
    <$> pure mempty -- program-options stanza
    <*> pure mempty -- program-locations stanza
    <*> monoidalFieldAla "extra-prog-path" (alaNubList' FSep FilePathNT) L.packageConfigProgramPathExtra
    <*> monoidalField "flags" L.packageConfigFlagAssignment
    <*> optionalFieldDef "library-vanilla" L.packageConfigVanillaLib mempty
    <*> optionalFieldDef "shared" L.packageConfigSharedLib mempty
    <*> optionalFieldDef "static" L.packageConfigStaticLib mempty
    <*> optionalFieldDef "executable-dynamic" L.packageConfigDynExe mempty
    <*> optionalFieldDef "executable-static" L.packageConfigFullyStaticExe mempty
    <*> optionalFieldDef "profiling" L.packageConfigProf mempty
    <*> optionalFieldDef "library-profiling" L.packageConfigProfLib mempty
    <*> optionalFieldDef "executable-profiling" L.packageConfigProfExe mempty
    <*> optionalFieldDef "profiling-detail" L.packageConfigProfDetail mempty
    <*> optionalFieldDef "library-profiling-detail" L.packageConfigProfLibDetail mempty
    <*> monoidalFieldAla "configure-options" (alaList' NoCommaFSep Token) L.packageConfigConfigureArgs
    <*> optionalFieldDef "optimization" L.packageConfigOptimization mempty
    <*> optionalFieldDef "program-prefix" L.packageConfigProgPrefix mempty
    <*> optionalFieldDef "program-suffix" L.packageConfigProgSuffix mempty
    <*> monoidalFieldAla "extra-lib-dirs" (alaList' FSep FilePathNT) L.packageConfigExtraLibDirs
    <*> monoidalFieldAla "extra-lib-dirs-static" (alaList' FSep FilePathNT) L.packageConfigExtraLibDirsStatic
    <*> monoidalFieldAla "extra-framework-dirs" (alaList' FSep FilePathNT) L.packageConfigExtraFrameworkDirs
    <*> monoidalFieldAla "extra-include-dirs" (alaList' FSep FilePathNT) L.packageConfigExtraIncludeDirs
    <*> optionalFieldDef "library-for-ghci" L.packageConfigGHCiLib mempty
    <*> optionalFieldDef "split-sections" L.packageConfigSplitSections mempty
    <*> optionalFieldDef "split-objs" L.packageConfigSplitObjs mempty
    <*> optionalFieldDef "executable-stripping" L.packageConfigStripExes mempty
    <*> optionalFieldDef "library-stripping" L.packageConfigStripLibs mempty
    <*> optionalFieldDef "tests" L.packageConfigTests mempty
    <*> optionalFieldDef "benchmarks" L.packageConfigBenchmarks mempty
    <*> packageConfigCoverageGrammar
    <*> optionalFieldDef "relocatable" L.packageConfigRelocatable mempty
    <*> optionalFieldDef "debug-info" L.packageConfigDebugInfo mempty
    <*> optionalFieldDef "build-info" L.packageConfigDumpBuildInfo mempty
    <*> optionalFieldDef "run-tests" L.packageConfigRunTests mempty
    <*> optionalFieldDef "documentation" L.packageConfigDocumentation mempty
    <*> optionalFieldDef "haddock-hoogle" L.packageConfigHaddockHoogle mempty
    <*> optionalFieldDef "haddock-html" L.packageConfigHaddockHtml mempty
    <*> optionalFieldDefAla "haddock-html-location" (alaFlag Token) L.packageConfigHaddockHtmlLocation mempty
    <*> optionalFieldDef "haddock-foreign-libraries" L.packageConfigHaddockForeignLibs mempty
    <*> optionalFieldDef "haddock-executables" L.packageConfigHaddockExecutables mempty
    <*> optionalFieldDef "haddock-tests" L.packageConfigHaddockTestSuites mempty
    <*> optionalFieldDef "haddock-benchmarks" L.packageConfigHaddockBenchmarks mempty
    <*> optionalFieldDef "haddock-internal" L.packageConfigHaddockInternal mempty
    <*> optionalFieldDefAla "haddock-css" (alaFlag FilePathNT) L.packageConfigHaddockCss mempty
    <*> optionalFieldDef "haddock-hyperlink-source" L.packageConfigHaddockLinkedSource mempty
    <*> optionalFieldDef "haddock-quickjump" L.packageConfigHaddockQuickJump mempty
    <*> optionalFieldDefAla "haddock-hscolour-css" (alaFlag FilePathNT) L.packageConfigHaddockHscolourCss mempty
    <*> optionalFieldDef "haddock-contents-location" L.packageConfigHaddockContents mempty
    <*> optionalFieldDef "haddock-index-location" L.packageConfigHaddockIndex mempty
    <*> optionalFieldDefAla "haddock-base-url" (alaFlag Token) L.packageConfigHaddockBaseUrl mempty
    <*> optionalFieldDefAla "haddock-lib" (alaFlag Token) L.packageConfigHaddockLib mempty
    <*> optionalFieldDefAla "haddock-output-dir" (alaFlag FilePathNT) L.packageConfigHaddockOutputDir mempty
    <*> optionalFieldDef "haddock-for-hackage" L.packageConfigHaddockForHackage mempty
    <*> optionalFieldDef "test-log" L.packageConfigTestHumanLog mempty
    <*> optionalFieldDef "test-machine-log" L.packageConfigTestMachineLog mempty
    <*> optionalFieldDef "test-show-details" L.packageConfigTestShowDetails mempty
    <*> optionalFieldDef "test-keep-tix-files" L.packageConfigTestKeepTix mempty
    <*> optionalFieldDefAla "test-wrapper" (alaFlag FilePathNT) L.packageConfigTestWrapper mempty
    <*> optionalFieldDef "test-fail-when-no-test-suites" L.packageConfigTestFailWhenNoTestSuites mempty
    <*> monoidalFieldAla "test-options" (alaList NoCommaFSep) L.packageConfigTestTestOptions
    <*> monoidalFieldAla "benchmark-options" (alaList NoCommaFSep) L.packageConfigBenchmarkOptions
    -- A PackageConfig may contain -options and -location fields inside a package * (packageConfigAllPackages) or package <name> stanza (packageConfigSpecificPackage).
    -- When declared at top level (packageConfigLocalPackages), the PackageConfig must contain a program-options stanza/program-locations for these fields.
    <* traverse_ (knownField . BS.pack . (<> "-options")) knownPrograms
    <* traverse_ (knownField . BS.pack . (<> "-location")) knownPrograms

packageConfigCoverageGrammar :: ParsecFieldGrammar PackageConfig (Distribution.Simple.Flag.Flag Bool)
packageConfigCoverageGrammar =
  (<>)
    <$> optionalFieldDef "library-coverage" L.packageConfigCoverage mempty
      ^^^ deprecatedSince CabalSpecV1_22 "Please use 'coverage' field instead."
    <*> optionalFieldDef "coverage" L.packageConfigCoverage mempty
