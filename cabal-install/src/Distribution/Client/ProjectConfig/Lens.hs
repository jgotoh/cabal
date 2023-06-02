module Distribution.Client.ProjectConfig.Lens where

import Distribution.Client.ProjectConfig.Types (MapMappend, PackageConfig, ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigProvenance, ProjectConfigShared)
import qualified Distribution.Client.ProjectConfig.Types as T
import Distribution.Client.Types.SourceRepo (SourceRepoList)
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.Package
  ( PackageId
  , PackageName
  , UnitId
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
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint
  )
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

projectConfigSummaryFile' :: Lens' ProjectConfigBuildOnly [PathTemplate]
projectConfigSummaryFile' f s = fmap (\x -> s{T.projectConfigSummaryFile = toNubList x}) (f (fromNubList $ T.projectConfigSummaryFile s))
{-# INLINEABLE projectConfigSummaryFile' #-}
