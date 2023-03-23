-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar (projectConfigSkeletonFieldGrammar) where

import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton)
import Distribution.FieldGrammar

projectConfigSkeletonFieldGrammar
    :: ( FieldGrammar c g, Applicative (g ProjectConfigSkeleton)
       )
    => g ProjectConfigSkeleton ProjectConfigSkeleton
projectConfigSkeletonFieldGrammar = undefined
