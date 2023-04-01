module Distribution.Client.ProjectConfig.Lens where

import Distribution.Compat.Prelude
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigBuildOnly, ProjectConfigShared, ProjectConfigProvenance, PackageConfig, MapMappend)
import Distribution.Compat.Lens
import Distribution.Client.Types.SourceRepo (SourceRepoList)
import qualified Distribution.Client.ProjectConfig.Types as T
import Distribution.Package
         ( PackageName, PackageId, UnitId )
import Distribution.Types.PackageVersionConstraint
         ( PackageVersionConstraint )

projectPackages :: Lens' ProjectConfig [String]
projectPackages f s = fmap (\x -> s { T.projectPackages = x }) (f (T.projectPackages s))
{-# INLINE projectPackages #-}

projectPackagesOptional :: Lens' ProjectConfig [String]
projectPackagesOptional f s = fmap (\x -> s { T.projectPackagesOptional = x }) (f (T.projectPackagesOptional s))
{-# INLINE projectPackagesOptional #-}

projectPackagesRepo :: Lens' ProjectConfig [SourceRepoList]
projectPackagesRepo f s = fmap (\x -> s { T.projectPackagesRepo = x }) (f (T.projectPackagesRepo s))
{-# INLINE projectPackagesRepo #-}

projectPackagesNamed :: Lens' ProjectConfig [PackageVersionConstraint]
projectPackagesNamed f s = fmap (\x -> s { T.projectPackagesNamed = x }) (f (T.projectPackagesNamed s))
{-# INLINE projectPackagesNamed #-}

projectConfigBuildOnly :: Lens' ProjectConfig ProjectConfigBuildOnly
projectConfigBuildOnly f s = fmap (\x -> s { T.projectConfigBuildOnly = x }) (f (T.projectConfigBuildOnly s))
{-# INLINE projectConfigBuildOnly #-}

projectConfigShared :: Lens' ProjectConfig ProjectConfigShared
projectConfigShared f s = fmap (\x -> s { T.projectConfigShared = x }) (f (T.projectConfigShared s))
{-# INLINE projectConfigShared #-}

projectConfigProvenance :: Lens' ProjectConfig (Set ProjectConfigProvenance)
projectConfigProvenance f s = fmap (\x -> s { T.projectConfigProvenance = x }) (f (T.projectConfigProvenance s))
{-# INLINE projectConfigProvenance #-}

projectConfigAllPackages :: Lens' ProjectConfig PackageConfig
projectConfigAllPackages f s = fmap (\x -> s { T.projectConfigAllPackages = x }) (f (T.projectConfigAllPackages s))
{-# INLINE projectConfigAllPackages #-}

projectConfigLocalPackages :: Lens' ProjectConfig PackageConfig
projectConfigLocalPackages f s = fmap (\x -> s { T.projectConfigLocalPackages = x }) (f (T.projectConfigLocalPackages s))
{-# INLINE projectConfigLocalPackages #-}

projectConfigSpecificPackage :: Lens' ProjectConfig (MapMappend PackageName PackageConfig)
projectConfigSpecificPackage f s = fmap (\x -> s { T.projectConfigSpecificPackage = x }) (f (T.projectConfigSpecificPackage s))
{-# INLINE projectConfigSpecificPackage #-}
