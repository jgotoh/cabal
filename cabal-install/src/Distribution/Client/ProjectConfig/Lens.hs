module Distribution.Client.ProjectConfig.Lens where

import Distribution.Client.ProjectConfig.Types (ProjectConfig (..))
import Distribution.Compat.Lens
import qualified Distribution.Client.ProjectConfig.Types as T

projectPackages :: Lens' ProjectConfig [String]
projectPackages f s = fmap (\x -> s { T.projectPackages = x }) (f (T.projectPackages s))
{-# INLINE projectPackages #-}

projectPackagesOptional :: Lens' ProjectConfig [String]
projectPackagesOptional f s = fmap (\x -> s { T.projectPackagesOptional = x }) (f (T.projectPackagesOptional s))
{-# INLINE projectPackagesOptional #-}
