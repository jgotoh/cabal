-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar (
  projectConfigFieldGrammar
  ) where

import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton)
import Distribution.Client.ProjectConfig.Types (ProjectConfig)
import Distribution.FieldGrammar

projectConfigFieldGrammar :: ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar = undefined
