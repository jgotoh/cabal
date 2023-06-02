{-# LANGUAGE OverloadedStrings #-}

-- | 'ProjectConfig' Field descriptions
module Distribution.Client.ProjectConfig.FieldGrammar
  ( projectConfigFieldGrammar
  ) where

import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigBuildOnly (..))
import Distribution.Client.Utils.Parsec
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.Simple.Flag
import Distribution.Types.PackageVersionConstraint (PackageVersionConstraint (..))
import Distribution.Verbosity

projectConfigFieldGrammar :: ParsecFieldGrammar' ProjectConfig
projectConfigFieldGrammar =
  ProjectConfig
    <$> monoidalFieldAla "packages" (alaList' FSep Token') L.projectPackages
    <*> monoidalFieldAla "optional-packages" (alaList' FSep Token') L.projectPackagesOptional
    <*> pure mempty -- source-repository-package stanza
    <*> monoidalFieldAla "extra-packages" formatPackagesNamedList L.projectPackagesNamed
    <*> blurFieldGrammar L.projectConfigBuildOnly projectConfigBuildOnlyFieldGrammar
    <*> pure mempty
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
    <*> pure (toFlag False) -- cli flag: projectConfigDryRun
    <*> pure (toFlag False) -- cli flag: projectConfigOnlyDeps
    <*> pure (toFlag False) -- cli flag: projectConfigOnlyDownload
    <*> monoidalFieldAla "build-summary" (alaNubList VCat) L.projectConfigSummaryFile
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
    <*> undefined
