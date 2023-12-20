{-# LANGUAGE OverloadedStrings #-}

-- | Parsing project configuration.
module Distribution.Client.ProjectConfig.Parsec
  ( -- * Package configuration
    parseProjectSkeleton
  , ProjectConfigSkeleton
  , ProjectConfig (..)

    -- ** Parsing
  , ParseResult
  , runParseResult
  ) where

import Control.Monad.State.Strict (StateT, execStateT, lift, modify)
import qualified Data.Map.Strict as Map
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Parsec (NamelessField (..), namelessFieldAnn)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))

-- TODO #6101 .Legacy -> ProjectConfigSkeleton should probably be moved here
import Distribution.Client.ProjectConfig.FieldGrammar (projectConfigFieldGrammar)
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigImport, ProjectConfigSkeleton)
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (MapLast (..), MapMappend (..), PackageConfig (..), ProjectConfig (..))
import Distribution.Client.Types.SourceRepo (SourceRepoList, sourceRepositoryPackageGrammar)
import Distribution.Fields.ConfVar (parseConditionConfVar)
import Distribution.Fields.ParseResult

-- AST type
import Distribution.Fields (Field, FieldLine, FieldName, Name (..), readFields')
import Distribution.Fields.LexerMonad (LexWarning, toPWarnings)
import Distribution.PackageDescription.Quirks (patchQuirks)
import Distribution.Parsec (CabalParsing, ParsecParser, explicitSimpleParsec, parsec, parsecToken, simpleParsecBS)
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Warning (PWarnType (..))
import Distribution.Simple.Program.Db (ProgramDb, knownPrograms, lookupKnownProgram)
import Distribution.Simple.Program.Types (programName)
import Distribution.Types.CondTree (CondBranch (..), CondTree (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Utils.Generic (breakMaybe, fromUTF8BS, toUTF8BS, unfoldrM, validateUTF8)

import qualified Data.ByteString as BS
import qualified Distribution.Compat.CharParsing as P
import qualified Text.Parsec

-- | Preprocess file and start parsing
parseProjectSkeleton :: FilePath -> BS.ByteString -> ParseResult ProjectConfigSkeleton
parseProjectSkeleton source bs = do
  case readFields' bs' of
    Right (fs, lexWarnings) -> do
      parseWarnings (toPWarnings lexWarnings)
      for_ invalidUtf8 $ \pos ->
        parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
      parseCondTree programDb source fs
    Left perr -> parseFatalFailure pos (show perr)
      where
        ppos = Text.Parsec.errorPos perr
        pos = Position (Text.Parsec.sourceLine ppos) (Text.Parsec.sourceColumn ppos)
  where
    invalidUtf8 = validateUTF8 bs
    bs' = case invalidUtf8 of
      Nothing -> bs
      Just _ -> toUTF8BS (fromUTF8BS bs)
    programDb = undefined

-- List of conditional blocks
newtype Conditional ann = Conditional [Section ann]
  deriving (Eq, Show)

-- | Separate valid conditional blocks from other sections so
-- all conditionals form their own groups.
-- TODO implement
partitionConditionals :: [[Section ann]] -> ([Section ann], [Conditional ann])
partitionConditionals sections = (concat sections, [])

parseCondTree
  :: ProgramDb
  -> FilePath
  -> [Field Position]
  -> ParseResult ProjectConfigSkeleton
parseCondTree programDb source fields0 = do
  -- sectionGroups are groups of sections between fields
  let (fs, sectionGroups) = partitionFields fields0
      (sections, conditionals) = partitionConditionals sectionGroups
      msg = show sectionGroups
  imports <- parseImports fs
  config <- parseFieldGrammar cabalSpec fs (projectConfigFieldGrammar source)
  config' <- view stateConfig <$> execStateT (goSections programDb sections) (SectionS config)
  let configSkeleton = CondNode config' imports []
  -- TODO parse conditionals
  return configSkeleton

-- Monad in which sections are parsed
type SectionParser = StateT SectionS ParseResult

-- | State of section parser
newtype SectionS = SectionS
  { _stateConfig :: ProjectConfig
  }

stateConfig :: Lens' SectionS ProjectConfig
stateConfig f (SectionS cfg) = SectionS <$> f cfg
{-# INLINEABLE stateConfig #-}

goSections :: ProgramDb -> [Section Position] -> SectionParser ()
goSections programDb = traverse_ (parseSection programDb)

parseSection :: ProgramDb -> Section Position -> SectionParser ()
parseSection programDb (MkSection (Name pos name) args secFields)
  | name == "source-repository-package" = do
      -- TODO implement syntaxError lineno "the section 'source-repository-package' takes no arguments"
      let (fields, secs) = partitionFields secFields
      srp <- lift $ parseFieldGrammar cabalSpec fields sourceRepositoryPackageGrammar
      stateConfig . L.projectPackagesRepo %= (++ [srp])
      unless (null secs) (warnInvalidSubsection pos name)
  | name == "program-options" = do
      -- TODO implement syntaxError lineno "the section 'program-options' takes no arguments"
      let (fields, secs) = partitionFields secFields
      opts <- lift $ parseProgramArgs programDb fields
      stateConfig . L.projectConfigLocalPackages %= (\lp -> lp{packageConfigProgramArgs = opts})
      unless (null secs) (warnInvalidSubsection pos name)
  | otherwise = do
      warnInvalidSubsection pos name

warnInvalidSubsection pos name = lift $ parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name

-- TODO implement, caution: check for cyclical imports
parseImports :: Fields Position -> ParseResult [ProjectConfigImport]
parseImports fs = return mempty

-- | Parse fields of a program-options stanza.
parseProgramArgs :: ProgramDb -> Fields Position -> ParseResult (MapMappend String [String])
parseProgramArgs programDb fields = foldM foldField mempty (Map.toList fields)
  where
    foldField accum (fieldName, fieldValues) = do
      case readProgramName programDb fieldName of
        Nothing -> warnUnknownFields fieldName fieldValues >> return accum
        Just program -> do
          args <- parseProgramArgsField fieldName fieldValues
          return $ accum <> (MapMappend $ Map.singleton program args)

-- | Parse all arguments to a single program in program-options stanza.
-- By processing '[NamelessField Position]', we support multiple occurrences of the field, concatenating the arguments.
parseProgramArgsField :: FieldName -> [NamelessField Position] -> ParseResult ([String])
parseProgramArgsField fieldName fieldValues =
  concat <$> mapM (\(MkNamelessField pos lines') -> parseProgramArgsFieldLines pos lines') fieldValues

-- | Parse all fieldLines of a single field occurrence in a program-options stanza.
parseProgramArgsFieldLines :: Position -> [FieldLine Position] -> ParseResult [String]
parseProgramArgsFieldLines pos = runFieldParser pos programArgsFieldParser cabalSpec

programArgsFieldParser :: CabalParsing m => m [String]
programArgsFieldParser = parseSep (Proxy :: Proxy FSep) parsecToken

-- | Extract the program name of a <progname>-options field and check whether it is known in the 'ProgramDb'.
readProgramName :: ProgramDb -> FieldName -> Maybe String
readProgramName programDb fieldName =
  parseProgramName fieldName >>= ((flip lookupKnownProgram) programDb) >>= pure . programName

parseProgramName :: FieldName -> Maybe String
parseProgramName fieldName = (explicitSimpleParsec parser name)
  where
    name = show fieldName
    parser :: ParsecParser String
    parser = P.manyTill P.anyChar (P.string "-options")

-- | Issue a 'PWTUnknownField' warning at all occurrences of a field.
warnUnknownFields :: FieldName -> [NamelessField Position] -> ParseResult ()
warnUnknownFields fieldName fieldLines = for_ fieldLines (\field -> parseWarning (pos field) PWTUnknownField message)
  where
    message = "Unknown field: " ++ show fieldName
    pos = namelessFieldAnn

cabalSpec :: CabalSpecVersion
cabalSpec = cabalSpecLatest
