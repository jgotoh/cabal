{-# LANGUAGE OverloadedStrings #-}

-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar
  ( projectConfigFieldGrammar
  ) where

import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigShared (..))
import Distribution.Client.Utils.Parsec
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.Simple.Flag
import Distribution.Solver.Types.ConstraintSource (ConstraintSource)
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Verbosity

projectConfigFieldGrammar :: ConstraintSource -> ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar constraintSrc =
  ProjectConfig
    <$> monoidalFieldAla "packages" (alaList' FSep Token') L.projectPackages
    <*> monoidalFieldAla "optional-packages" (alaList' FSep Token') L.projectPackagesOptional
    <*> pure mempty -- source-repository-package stanza
    <*> monoidalFieldAla "extra-packages" formatPackagesNamedList L.projectPackagesNamed
    <*> blurFieldGrammar L.projectConfigBuildOnly projectConfigBuildOnlyFieldGrammar
    <*> blurFieldGrammar L.projectConfigShared (projectConfigSharedFieldGrammar constraintSrc)
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty
    <*> pure mempty

formatPackagesNamedList :: [PackageVersionConstraint] -> List CommaVCat (Identity PackageVersionConstraint) PackageVersionConstraint
formatPackagesNamedList = alaList CommaVCat

projectConfigBuildOnlyFieldGrammar :: ParsecFieldGrammar' ProjectConfigBuildOnly
projectConfigBuildOnlyFieldGrammar =
  ProjectConfigBuildOnly
    <$> optionalFieldDef "verbose" L.projectConfigVerbosity (pure normal)
    <*> pure mempty -- cli flag: projectConfigDryRun
    <*> pure mempty -- cli flag: projectConfigOnlyDeps
    <*> pure mempty -- cli flag: projectConfigOnlyDownload
    <*> monoidalFieldAla "build-summary" (alaNubList VCat) L.projectConfigSummaryFile
    <*> optionalFieldDef "build-log" L.projectConfigLogFile mempty
    <*> pure mempty -- cli flag: projectConfigBuildReports
    <*> optionalFieldDef "report-planning-failure" L.projectConfigReportPlanningFailure mempty
    <*> monoidalFieldAla "symlink-bindir" (alaFlag FilePathNT) L.projectConfigSymlinkBinDir
    <*> monoidalFieldAla "jobs" (alaFlag NumJobs) L.projectConfigNumJobs
    <*> optionalFieldDef "keep-going" L.projectConfigKeepGoing mempty
    <*> optionalFieldDef "offline" L.projectConfigOfflineMode mempty
    <*> optionalFieldDef "haddock-keep-temp-files" L.projectConfigKeepTempFiles mempty
    <*> monoidalFieldAla "http-transport" (alaFlag Token) L.projectConfigHttpTransport
    <*> optionalFieldDef "ignore-expiry" L.projectConfigIgnoreExpiry mempty
    <*> monoidalFieldAla "remote-repo-cache" (alaFlag FilePathNT) L.projectConfigCacheDir
    <*> monoidalFieldAla "logs-dir" (alaFlag FilePathNT) L.projectConfigLogsDir
    <*> pure mempty

projectConfigSharedFieldGrammar :: ConstraintSource -> ParsecFieldGrammar' ProjectConfigShared
projectConfigSharedFieldGrammar constraintSrc =
  ProjectConfigShared
    <$> pure mempty -- cli flag: projectConfigDistDir
    <*> pure mempty -- cli flag: projectConfigConfigFile
    <*> pure mempty -- cli flag: projectConfigProjectDir
    <*> pure mempty -- cli flag: projectConfigProjectFile
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
    <*> pure mempty -- cli flag: projectConfigStoreDir
    <*> monoidalFieldAla "constraints" (alaList' FSep ProjectConstraints) L.projectConfigConstraints
      ^^^ (fmap . fmap) (\(userConstraint, _) -> (userConstraint, constraintSrc))
    <*> pure undefined -- projectConfigPreferences [PackageVersionConstraint]
    <*> pure undefined -- projectConfigCabalVersion Flag Version
    <*> pure undefined -- projectConfigSolver Flag PreSolver
    <*> pure undefined -- projectConfigAllowOlder Maybe AllowOlder
    <*> pure undefined -- projectConfigAllowNewer Maybe AllowNewer
    <*> pure undefined -- projectConfigWriteGhcEnvironmentFilesPolicy Flag WriteGhcEnvironmentFilesPolicy
    <*> pure undefined -- projectConfigMaxBackjumps Flag Int
    <*> pure undefined -- projectConfigReorderGoals Flag ReorderGoals
    <*> pure undefined -- projectConfigCountConflicts Flag CountConflicts
    <*> pure undefined -- projectConfigFineGrainedConflicts Flag FineGrainedConflicts
    <*> pure undefined -- projectConfigMinimizeConflictSet Flag MinimizeConflictSet
    <*> pure undefined -- projectConfigStrongFlags Flag StrongFlags
    <*> pure undefined -- projectConfigAllowBootLibInstalls Flag AllowBootLibInstalls
    <*> pure undefined -- projectConfigOnlyConstrained Flag OnlyConstrained
    <*> pure undefined -- projectConfigPerComponent Flag Bool
    <*> pure undefined -- projectConfigIndependentGoals Flag IndependentGoals
    <*> pure undefined -- projectConfigPreferOldest Flag PreferOldest
    <*> pure undefined -- projectConfigProgPathExtra NubList FilePath
    <*> pure undefined -- projectConfigMultiRepl Flag Bool
