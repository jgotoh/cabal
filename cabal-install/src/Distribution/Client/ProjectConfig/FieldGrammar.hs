{-# LANGUAGE OverloadedStrings     #-}
-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar (
  projectConfigFieldGrammar
  ) where

import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton)
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..))
import Distribution.FieldGrammar

projectConfigFieldGrammar :: ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar = ProjectConfig
  <$> monoidalFieldAla    "packages"            (alaList' FSep Token')      L.projectPackages
  <*> monoidalFieldAla    "optional-packages"   (alaList' FSep Token')      L.projectPackagesOptional
  <*> pure mempty
  <*> pure mempty
  <*> pure mempty
  <*> pure mempty
  <*> pure mempty
  <*> pure mempty
  <*> pure mempty
  <*> pure mempty
