{-# LANGUAGE OverloadedStrings   #-}

-- | Parsing project configuration.

module Distribution.Client.ProjectConfig.Parsec (
  -- * Package configuration
  parseProjectSkeleton,

  -- ** Parsing
  ParseResult,
  runParseResult
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
-- TODO #6101 .Legacy -> ProjectConfigSkeleton should probably be moved here
import Distribution.Client.ProjectConfig.FieldGrammar (projectConfigFieldGrammar)
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton, ProjectConfigImport)
import Distribution.Client.ProjectConfig.Types (ProjectConfig)
import Distribution.Fields.ConfVar                   (parseConditionConfVar)
import Distribution.Fields.ParseResult
-- AST type
import Distribution.Fields (Field, readFields', Name (..))
import Distribution.Fields.LexerMonad                (LexWarning, toPWarnings)
import Distribution.PackageDescription.Quirks        (patchQuirks)
import Distribution.Parsec                           (parsec, simpleParsecBS)
import Distribution.Parsec.Position                  (Position (..), zeroPos)
import Distribution.Parsec.Warning                   (PWarnType (..))
import Distribution.Types.CondTree                   (CondTree (..), CondBranch (..))
import Distribution.Types.ConfVar                    (ConfVar (..))
import Distribution.Utils.Generic                    (breakMaybe, fromUTF8BS, toUTF8BS, unfoldrM, validateUTF8)

import qualified Data.ByteString                                   as BS
import qualified Text.Parsec                                       as P

-- TODO 6101 the following is copied from Distribution.PackageDescription.Parsec.parseGenericPackageDescription
-- - do we need to use patchQuirks also for cabal.project files? if we do, we can extract a
-- common preprocessing function (patchQuirks, valid UTF8, ) to avoid code duplication
-- | Preprocess file and start parsing
parseProjectSkeleton :: BS.ByteString -> ParseResult ProjectConfigSkeleton
parseProjectSkeleton bs = do
    case readFields' bs'' of
        Right (fs, lexWarnings) -> do
            when patched $
                parseWarning zeroPos PWTQuirkyCabalFile "Legacy cabal file"
            parseProjectSkeleton' lexWarnings invalidUtf8 fs
        Left perr -> parseFatalFailure pos (show perr) where
            ppos = P.errorPos perr
            pos  = Position (P.sourceLine ppos) (P.sourceColumn ppos)
  where
    (patched, bs') = patchQuirks bs
    invalidUtf8 = validateUTF8 bs'
    bs'' = case invalidUtf8 of
        Nothing -> bs'
        Just _  -> toUTF8BS (fromUTF8BS bs')

parseProjectSkeleton'
    :: [LexWarning]
    -> Maybe Int
    -> [Field Position]
    -> ParseResult ProjectConfigSkeleton
parseProjectSkeleton' lexWarnings utf8WarnPos fs = do
    parseWarnings (toPWarnings lexWarnings)
    for_ utf8WarnPos $ \pos ->
        parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos

    -- TODO I am not sure whether we need sectionizeFields for cabal.project parsing
    -- it is declared in Distribution.PackageDescription.Parsec.sectionizeFields
    -- and used to convert old-style Cabal files, does this also apply to cabal.project files?
    -- let (syntax, fs') = sectionizeFields fs
    -- maybe we can also just use partitionFields

    -- let (fields, sectionFields) = takeFields fs
    parseCondTree specVer hasElif projectConfigFieldGrammar fs
    where
      -- TODO where do we get specVer?
      specVer :: CabalSpecVersion
      specVer = CabalSpecV3_8
      hasElif = specHasElif specVer

-- migrated from Distribution.PackageDescription.Parsec
parseCondTree
    :: CabalSpecVersion
    -> HasElif                                    -- ^ accept @elif@
    -> ParsecFieldGrammar' ProjectConfig          -- ^ grammar
    -> [Field Position]
    -> ParseResult ProjectConfigSkeleton
parseCondTree v hasElif grammar = go
  where
    go fields0 = do
        let (fs, ss) = partitionFields fields0
        imports <- parseImports fs
        x <- parseFieldGrammar v fs grammar
        branches <- concat <$> traverse parseIfs ss
        return $ CondNode x imports branches

    parseIfs :: [Section Position] -> ParseResult [CondBranch ConfVar [ProjectConfigImport] ProjectConfig]
    parseIfs [] = return []
    parseIfs (MkSection (Name _ name) test fields : sections) | name == "if" = do
        test' <- parseConditionConfVar test
        fields' <- go fields
        (elseFields, sections') <- parseElseIfs sections
        return (CondBranch test' fields' elseFields : sections')
    parseIfs (MkSection (Name pos name) _ _ : sections) = do
        parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name
        parseIfs sections

    parseElseIfs
        :: [Section Position]
        -> ParseResult (Maybe (CondTree ConfVar [ProjectConfigImport] ProjectConfig), [CondBranch ConfVar [ProjectConfigImport] ProjectConfig])
    parseElseIfs [] = return (Nothing, [])
    parseElseIfs (MkSection (Name pos name) args fields : sections) | name == "else" = do
        unless (null args) $
            parseFailure pos $ "`else` section has section arguments " ++ show args
        elseFields <- go fields
        sections' <- parseIfs sections
        return (Just elseFields, sections')

    parseElseIfs (MkSection (Name _ name) test fields : sections) | hasElif == HasElif, name == "elif" = do
        test' <- parseConditionConfVar test
        fields' <- go fields
        (elseFields, sections') <- parseElseIfs sections
        -- we parse an empty 'Fields', to get empty value for a node
        a <- parseFieldGrammar v mempty grammar
        return (Just $ CondNode a mempty [CondBranch test' fields' elseFields], sections')

    parseElseIfs (MkSection (Name pos name) _ _ : sections) | name == "elif" = do
        parseWarning pos PWTInvalidSubsection $ "invalid subsection \"elif\". You should set cabal-version: 2.2 or larger to use elif-conditionals."
        (,) Nothing <$> parseIfs sections

    parseElseIfs sections = (,) Nothing <$> parseIfs sections

-- TODO implement, caution: check for cyclical imports
parseImports :: Fields Position -> ParseResult [ProjectConfigImport]
parseImports fs = return mempty
