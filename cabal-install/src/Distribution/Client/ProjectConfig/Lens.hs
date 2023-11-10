module Distribution.Client.ProjectConfig.Lens where

import Distribution.Client.Dependency.Types (PreSolver (..))
import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )
import Distribution.Client.IndexUtils.IndexState (TotalIndexState)
import Distribution.Client.ProjectConfig.Types (MapMappend, PackageConfig, ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigProvenance, ProjectConfigShared)
import qualified Distribution.Client.ProjectConfig.Types as T
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types.AllowNewer (AllowNewer, AllowOlder)
import Distribution.Client.Types.SourceRepo (SourceRepoList)
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy (WriteGhcEnvironmentFilesPolicy)
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Package
  ( PackageId
  , PackageName
  , UnitId
  )
import Distribution.Simple.Compiler
  ( Compiler
  , CompilerFlavor
  , DebugInfoLevel (..)
  , OptimisationLevel (..)
  , PackageDB
  , ProfDetailLevel
  )
import Distribution.Simple.InstallDirs
  ( InstallDirs
  , PathTemplate
  )
import Distribution.Simple.Setup
  ( DumpBuildInfo (..)
  , Flag
  , HaddockTarget (..)
  , TestShowDetails (..)
  )
import Distribution.Solver.Types.ConstraintSource (ConstraintSource)
import Distribution.Solver.Types.Settings (AllowBootLibInstalls (..), CountConflicts (..), FineGrainedConflicts (..), MinimizeConflictSet (..), PreferOldest (..), ReorderGoals (..), StrongFlags (..))
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint
  )
import Distribution.Types.Version (Version)
import Distribution.Utils.NubList
  ( NubList
  , fromNubList
  , toNubList
  )
import Distribution.Verbosity

projectPackages :: Lens' ProjectConfig [String]
projectPackages f s = fmap (\x -> s{T.projectPackages = x}) (f (T.projectPackages s))
{-# INLINEABLE projectPackages #-}

projectPackagesOptional :: Lens' ProjectConfig [String]
projectPackagesOptional f s = fmap (\x -> s{T.projectPackagesOptional = x}) (f (T.projectPackagesOptional s))
{-# INLINEABLE projectPackagesOptional #-}

projectPackagesRepo :: Lens' ProjectConfig [SourceRepoList]
projectPackagesRepo f s = fmap (\x -> s{T.projectPackagesRepo = x}) (f (T.projectPackagesRepo s))
{-# INLINEABLE projectPackagesRepo #-}

projectPackagesNamed :: Lens' ProjectConfig [PackageVersionConstraint]
projectPackagesNamed f s = fmap (\x -> s{T.projectPackagesNamed = x}) (f (T.projectPackagesNamed s))
{-# INLINEABLE projectPackagesNamed #-}

projectConfigBuildOnly :: Lens' ProjectConfig ProjectConfigBuildOnly
projectConfigBuildOnly f s = fmap (\x -> s{T.projectConfigBuildOnly = x}) (f (T.projectConfigBuildOnly s))
{-# INLINEABLE projectConfigBuildOnly #-}

projectConfigShared :: Lens' ProjectConfig ProjectConfigShared
projectConfigShared f s = fmap (\x -> s{T.projectConfigShared = x}) (f (T.projectConfigShared s))
{-# INLINEABLE projectConfigShared #-}

projectConfigProvenance :: Lens' ProjectConfig (Set ProjectConfigProvenance)
projectConfigProvenance f s = fmap (\x -> s{T.projectConfigProvenance = x}) (f (T.projectConfigProvenance s))
{-# INLINEABLE projectConfigProvenance #-}

projectConfigAllPackages :: Lens' ProjectConfig PackageConfig
projectConfigAllPackages f s = fmap (\x -> s{T.projectConfigAllPackages = x}) (f (T.projectConfigAllPackages s))
{-# INLINEABLE projectConfigAllPackages #-}

projectConfigLocalPackages :: Lens' ProjectConfig PackageConfig
projectConfigLocalPackages f s = fmap (\x -> s{T.projectConfigLocalPackages = x}) (f (T.projectConfigLocalPackages s))
{-# INLINEABLE projectConfigLocalPackages #-}

projectConfigSpecificPackage :: Lens' ProjectConfig (MapMappend PackageName PackageConfig)
projectConfigSpecificPackage f s = fmap (\x -> s{T.projectConfigSpecificPackage = x}) (f (T.projectConfigSpecificPackage s))
{-# INLINEABLE projectConfigSpecificPackage #-}

projectConfigVerbosity :: Lens' ProjectConfigBuildOnly (Flag Verbosity)
projectConfigVerbosity f s = fmap (\x -> s{T.projectConfigVerbosity = x}) (f (T.projectConfigVerbosity s))
{-# INLINEABLE projectConfigVerbosity #-}

projectConfigSummaryFile :: Lens' ProjectConfigBuildOnly (NubList PathTemplate)
projectConfigSummaryFile f s = fmap (\x -> s{T.projectConfigSummaryFile = x}) (f (T.projectConfigSummaryFile s))
{-# INLINEABLE projectConfigSummaryFile #-}

projectConfigLogFile :: Lens' ProjectConfigBuildOnly (Flag PathTemplate)
projectConfigLogFile f s = fmap (\x -> s{T.projectConfigLogFile = x}) (f (T.projectConfigLogFile s))
{-# INLINEABLE projectConfigLogFile #-}

projectConfigReportPlanningFailure :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigReportPlanningFailure f s = fmap (\x -> s{T.projectConfigReportPlanningFailure = x}) (f (T.projectConfigReportPlanningFailure s))
{-# INLINEABLE projectConfigReportPlanningFailure #-}

projectConfigSymlinkBinDir :: Lens' ProjectConfigBuildOnly (Flag FilePath)
projectConfigSymlinkBinDir f s = fmap (\x -> s{T.projectConfigSymlinkBinDir = x}) (f (T.projectConfigSymlinkBinDir s))
{-# INLINEABLE projectConfigSymlinkBinDir #-}

projectConfigNumJobs :: Lens' ProjectConfigBuildOnly (Flag (Maybe Int))
projectConfigNumJobs f s = fmap (\x -> s{T.projectConfigNumJobs = x}) (f (T.projectConfigNumJobs s))
{-# INLINEABLE projectConfigNumJobs #-}

projectConfigUseSemaphore :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigUseSemaphore f s = fmap (\x -> s{T.projectConfigUseSemaphore = x}) (f (T.projectConfigUseSemaphore s))
{-# INLINEABLE projectConfigUseSemaphore #-}

projectConfigKeepGoing :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigKeepGoing f s = fmap (\x -> s{T.projectConfigKeepGoing = x}) (f (T.projectConfigKeepGoing s))
{-# INLINEABLE projectConfigKeepGoing #-}

projectConfigOfflineMode :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigOfflineMode f s = fmap (\x -> s{T.projectConfigOfflineMode = x}) (f (T.projectConfigOfflineMode s))
{-# INLINEABLE projectConfigOfflineMode #-}

projectConfigKeepTempFiles :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigKeepTempFiles f s = fmap (\x -> s{T.projectConfigKeepTempFiles = x}) (f (T.projectConfigKeepTempFiles s))
{-# INLINEABLE projectConfigKeepTempFiles #-}

projectConfigHttpTransport :: Lens' ProjectConfigBuildOnly (Flag String)
projectConfigHttpTransport f s = fmap (\x -> s{T.projectConfigHttpTransport = x}) (f (T.projectConfigHttpTransport s))
{-# INLINEABLE projectConfigHttpTransport #-}

projectConfigIgnoreExpiry :: Lens' ProjectConfigBuildOnly (Flag Bool)
projectConfigIgnoreExpiry f s = fmap (\x -> s{T.projectConfigIgnoreExpiry = x}) (f (T.projectConfigIgnoreExpiry s))
{-# INLINEABLE projectConfigIgnoreExpiry #-}

projectConfigCacheDir :: Lens' ProjectConfigBuildOnly (Flag FilePath)
projectConfigCacheDir f s = fmap (\x -> s{T.projectConfigCacheDir = x}) (f (T.projectConfigCacheDir s))
{-# INLINEABLE projectConfigCacheDir #-}

projectConfigLogsDir :: Lens' ProjectConfigBuildOnly (Flag FilePath)
projectConfigLogsDir f s = fmap (\x -> s{T.projectConfigLogsDir = x}) (f (T.projectConfigLogsDir s))
{-# INLINEABLE projectConfigLogsDir #-}

projectConfigIgnoreProject :: Lens' ProjectConfigShared (Flag Bool)
projectConfigIgnoreProject f s = fmap (\x -> s{T.projectConfigIgnoreProject = x}) (f (T.projectConfigIgnoreProject s))
{-# INLINEABLE projectConfigIgnoreProject #-}

projectConfigHcFlavor :: Lens' ProjectConfigShared (Flag CompilerFlavor)
projectConfigHcFlavor f s = fmap (\x -> s{T.projectConfigHcFlavor = x}) (f (T.projectConfigHcFlavor s))
{-# INLINEABLE projectConfigHcFlavor #-}

projectConfigHcPath :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigHcPath f s = fmap (\x -> s{T.projectConfigHcPath = x}) (f (T.projectConfigHcPath s))
{-# INLINEABLE projectConfigHcPath #-}

projectConfigHcPkg :: Lens' ProjectConfigShared (Flag FilePath)
projectConfigHcPkg f s = fmap (\x -> s{T.projectConfigHcPkg = x}) (f (T.projectConfigHcPkg s))
{-# INLINEABLE projectConfigHcPkg #-}

projectConfigHaddockIndex :: Lens' ProjectConfigShared (Flag PathTemplate)
projectConfigHaddockIndex f s = fmap (\x -> s{T.projectConfigHaddockIndex = x}) (f (T.projectConfigHaddockIndex s))
{-# INLINEABLE projectConfigHaddockIndex #-}

projectConfigPackageDBs :: Lens' ProjectConfigShared [Maybe PackageDB]
projectConfigPackageDBs f s = fmap (\x -> s{T.projectConfigPackageDBs = x}) (f (T.projectConfigPackageDBs s))
{-# INLINEABLE projectConfigPackageDBs #-}

projectConfigActiveRepos :: Lens' ProjectConfigShared (Flag ActiveRepos)
projectConfigActiveRepos f s = fmap (\x -> s{T.projectConfigActiveRepos = x}) (f (T.projectConfigActiveRepos s))
{-# INLINEABLE projectConfigActiveRepos #-}

projectConfigIndexState :: Lens' ProjectConfigShared (Flag TotalIndexState)
projectConfigIndexState f s = fmap (\x -> s{T.projectConfigIndexState = x}) (f (T.projectConfigIndexState s))
{-# INLINEABLE projectConfigIndexState #-}

projectConfigConstraints :: Lens' ProjectConfigShared [(UserConstraint, ConstraintSource)]
projectConfigConstraints f s = fmap (\x -> s{T.projectConfigConstraints = x}) (f (T.projectConfigConstraints s))
{-# INLINEABLE projectConfigConstraints #-}

projectConfigPreferences :: Lens' ProjectConfigShared [PackageVersionConstraint]
projectConfigPreferences f s = fmap (\x -> s{T.projectConfigPreferences = x}) (f (T.projectConfigPreferences s))
{-# INLINEABLE projectConfigPreferences #-}

projectConfigCabalVersion :: Lens' ProjectConfigShared (Flag Version)
projectConfigCabalVersion f s = fmap (\x -> s{T.projectConfigCabalVersion = x}) (f (T.projectConfigCabalVersion s))
{-# INLINEABLE projectConfigCabalVersion #-}

projectConfigSolver :: Lens' ProjectConfigShared (Flag PreSolver)
projectConfigSolver f s = fmap (\x -> s{T.projectConfigSolver = x}) (f (T.projectConfigSolver s))
{-# INLINEABLE projectConfigSolver #-}

projectConfigAllowOlder :: Lens' ProjectConfigShared (Maybe AllowOlder)
projectConfigAllowOlder f s = fmap (\x -> s{T.projectConfigAllowOlder = x}) (f (T.projectConfigAllowOlder s))
{-# INLINEABLE projectConfigAllowOlder #-}

projectConfigAllowNewer :: Lens' ProjectConfigShared (Maybe AllowNewer)
projectConfigAllowNewer f s = fmap (\x -> s{T.projectConfigAllowNewer = x}) (f (T.projectConfigAllowNewer s))
{-# INLINEABLE projectConfigAllowNewer #-}

projectConfigWriteGhcEnvironmentFilesPolicy :: Lens' ProjectConfigShared (Flag WriteGhcEnvironmentFilesPolicy)
projectConfigWriteGhcEnvironmentFilesPolicy f s = fmap (\x -> s{T.projectConfigWriteGhcEnvironmentFilesPolicy = x}) (f (T.projectConfigWriteGhcEnvironmentFilesPolicy s))
{-# INLINEABLE projectConfigWriteGhcEnvironmentFilesPolicy #-}

projectConfigMaxBackjumps :: Lens' ProjectConfigShared (Flag Int)
projectConfigMaxBackjumps f s = fmap (\x -> s{T.projectConfigMaxBackjumps = x}) (f (T.projectConfigMaxBackjumps s))
{-# INLINEABLE projectConfigMaxBackjumps #-}

projectConfigReorderGoals :: Lens' ProjectConfigShared (Flag ReorderGoals)
projectConfigReorderGoals f s = fmap (\x -> s{T.projectConfigReorderGoals = x}) (f (T.projectConfigReorderGoals s))
{-# INLINEABLE projectConfigReorderGoals #-}

projectConfigCountConflicts :: Lens' ProjectConfigShared (Flag CountConflicts)
projectConfigCountConflicts f s = fmap (\x -> s{T.projectConfigCountConflicts = x}) (f (T.projectConfigCountConflicts s))
{-# INLINEABLE projectConfigCountConflicts #-}

projectConfigFineGrainedConflicts :: Lens' ProjectConfigShared (Flag FineGrainedConflicts)
projectConfigFineGrainedConflicts f s = fmap (\x -> s{T.projectConfigFineGrainedConflicts = x}) (f (T.projectConfigFineGrainedConflicts s))
{-# INLINEABLE projectConfigFineGrainedConflicts #-}

projectConfigMinimizeConflictSet :: Lens' ProjectConfigShared (Flag MinimizeConflictSet)
projectConfigMinimizeConflictSet f s = fmap (\x -> s{T.projectConfigMinimizeConflictSet = x}) (f (T.projectConfigMinimizeConflictSet s))
{-# INLINEABLE projectConfigMinimizeConflictSet #-}

projectConfigStrongFlags :: Lens' ProjectConfigShared (Flag StrongFlags)
projectConfigStrongFlags f s = fmap (\x -> s{T.projectConfigStrongFlags = x}) (f (T.projectConfigStrongFlags s))
{-# INLINEABLE projectConfigStrongFlags #-}

projectConfigAllowBootLibInstalls :: Lens' ProjectConfigShared (Flag AllowBootLibInstalls)
projectConfigAllowBootLibInstalls f s = fmap (\x -> s{T.projectConfigAllowBootLibInstalls = x}) (f (T.projectConfigAllowBootLibInstalls s))
{-# INLINEABLE projectConfigAllowBootLibInstalls #-}

projectConfigPreferOldest :: Lens' ProjectConfigShared (Flag PreferOldest)
projectConfigPreferOldest f s = fmap (\x -> s{T.projectConfigPreferOldest = x}) (f (T.projectConfigPreferOldest s))
{-# INLINEABLE projectConfigPreferOldest #-}

projectConfigProgPathExtra :: Lens' ProjectConfigShared (NubList FilePath)
projectConfigProgPathExtra f s = fmap (\x -> s{T.projectConfigProgPathExtra = x}) (f (T.projectConfigProgPathExtra s))
{-# INLINEABLE projectConfigProgPathExtra #-}

projectConfigMultiRepl :: Lens' ProjectConfigShared (Flag Bool)
projectConfigMultiRepl f s = fmap (\x -> s{T.projectConfigMultiRepl = x}) (f (T.projectConfigMultiRepl s))
{-# INLINEABLE projectConfigMultiRepl #-}
