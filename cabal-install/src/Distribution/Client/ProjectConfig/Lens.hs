module Distribution.Client.ProjectConfig.Lens where

import Distribution.Compat.Prelude
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigShared, ProjectConfigProvenance, PackageConfig, MapMappend)
import Distribution.Compat.Lens
import Distribution.Client.Types.SourceRepo (SourceRepoList)
import qualified Distribution.Client.ProjectConfig.Types as T
import Distribution.Package
         ( PackageName, PackageId, UnitId )
import Distribution.Simple.Setup
         ( Flag, HaddockTarget(..), TestShowDetails(..), DumpBuildInfo (..) )
import Distribution.Types.PackageVersionConstraint
         ( PackageVersionConstraint )
import Distribution.Verbosity
import Distribution.Simple.InstallDirs
         ( PathTemplate, InstallDirs )
import Distribution.Utils.NubList
         ( NubList, toNubList, fromNubList )

projectPackages :: Lens' ProjectConfig [String]
projectPackages f s = fmap (\x -> s { T.projectPackages = x }) (f (T.projectPackages s))
{-# INLINABLE projectPackages #-}

projectPackagesOptional :: Lens' ProjectConfig [String]
projectPackagesOptional f s = fmap (\x -> s { T.projectPackagesOptional = x }) (f (T.projectPackagesOptional s))
{-# INLINABLE projectPackagesOptional #-}

projectPackagesRepo :: Lens' ProjectConfig [SourceRepoList]
projectPackagesRepo f s = fmap (\x -> s { T.projectPackagesRepo = x }) (f (T.projectPackagesRepo s))
{-# INLINABLE projectPackagesRepo #-}

projectPackagesNamed :: Lens' ProjectConfig [PackageVersionConstraint]
projectPackagesNamed f s = fmap (\x -> s { T.projectPackagesNamed = x }) (f (T.projectPackagesNamed s))
{-# INLINABLE projectPackagesNamed #-}

projectConfigBuildOnly :: Lens' ProjectConfig ProjectConfigBuildOnly
projectConfigBuildOnly f s = fmap (\x -> s { T.projectConfigBuildOnly = x }) (f (T.projectConfigBuildOnly s))
{-# INLINABLE projectConfigBuildOnly #-}

projectConfigShared :: Lens' ProjectConfig ProjectConfigShared
projectConfigShared f s = fmap (\x -> s { T.projectConfigShared = x }) (f (T.projectConfigShared s))
{-# INLINABLE projectConfigShared #-}

projectConfigProvenance :: Lens' ProjectConfig (Set ProjectConfigProvenance)
projectConfigProvenance f s = fmap (\x -> s { T.projectConfigProvenance = x }) (f (T.projectConfigProvenance s))
{-# INLINABLE projectConfigProvenance #-}

projectConfigAllPackages :: Lens' ProjectConfig PackageConfig
projectConfigAllPackages f s = fmap (\x -> s { T.projectConfigAllPackages = x }) (f (T.projectConfigAllPackages s))
{-# INLINABLE projectConfigAllPackages #-}

projectConfigLocalPackages :: Lens' ProjectConfig PackageConfig
projectConfigLocalPackages f s = fmap (\x -> s { T.projectConfigLocalPackages = x }) (f (T.projectConfigLocalPackages s))
{-# INLINABLE projectConfigLocalPackages #-}

projectConfigSpecificPackage :: Lens' ProjectConfig (MapMappend PackageName PackageConfig)
projectConfigSpecificPackage f s = fmap (\x -> s { T.projectConfigSpecificPackage = x }) (f (T.projectConfigSpecificPackage s))
{-# INLINABLE projectConfigSpecificPackage #-}

projectConfigVerbosity :: Lens' ProjectConfigBuildOnly (Flag Verbosity)
projectConfigVerbosity f s = fmap (\x -> s { T.projectConfigVerbosity = x }) (f (T.projectConfigVerbosity s))
{-# INLINABLE projectConfigVerbosity #-}

projectConfigSummaryFile :: Lens' ProjectConfigBuildOnly (NubList PathTemplate)
projectConfigSummaryFile f s = fmap (\x -> s { T.projectConfigSummaryFile = x }) (f (T.projectConfigSummaryFile s))
{-# INLINABLE projectConfigSummaryFile #-}

projectConfigSummaryFile' :: Lens' ProjectConfigBuildOnly [PathTemplate]
projectConfigSummaryFile' f s = fmap (\x -> s { T.projectConfigSummaryFile = toNubList x }) (f (fromNubList $ T.projectConfigSummaryFile s))
{-# INLINABLE projectConfigSummaryFile' #-}
