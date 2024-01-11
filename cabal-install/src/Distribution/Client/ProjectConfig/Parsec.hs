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
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
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
import Distribution.Parsec (CabalParsing, ParsecParser, explicitEitherParsec, parsec, parsecFilePath, parsecToken, runParsecParser, simpleParsecBS)
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Warning (PWarnType (..))
import Distribution.Simple.Program.Db (ProgramDb, defaultProgramDb, knownPrograms, lookupKnownProgram)
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
    -- TODO the legacy parser uses 'defaultProgramDb' in programLocationsFieldDescrs programOptionsFieldDescrs to parse the
    -- progname-options/-locations fields, so I used this too instead of parameterizing it. Is this okay?
    programDb = defaultProgramDb

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
      srp <- lift $ parseFieldGrammar cabalSpec fields sourceRepositoryPackageGrammar
      stateConfig . L.projectPackagesRepo %= (++ [srp])
      unless (null sections) (warnInvalidSubsection pos name)
  | name == "program-options" = do
      -- TODO implement syntaxError lineno "the section 'program-options' takes no arguments"
      opts <- lift $ parseProgramArgs programDb fields
      stateConfig . L.projectConfigLocalPackages %= (\lp -> lp{packageConfigProgramArgs = opts})
      unless (null sections) (warnInvalidSubsection pos name)
  | name == "program-locations" = do
      -- TODO implement syntaxError lineno "the section 'program-locations' takes no arguments"
      opts <- lift $ parseProgramPaths programDb fields
      stateConfig . L.projectConfigLocalPackages %= (\lp -> lp{packageConfigProgramPaths = opts})
      unless (null sections) (warnInvalidSubsection pos name)
  | otherwise = do
      warnInvalidSubsection pos name
  where
    (fields, sections) = partitionFields secFields

warnInvalidSubsection pos name = lift $ parseWarning pos PWTInvalidSubsection $ "invalid subsection " ++ show name

-- TODO implement, caution: check for cyclical imports
parseImports :: Fields Position -> ParseResult [ProjectConfigImport]
parseImports fs = return mempty

-- | Parse fields of a program-options stanza.
parseProgramArgs :: ProgramDb -> Fields Position -> ParseResult (MapMappend String [String])
parseProgramArgs programDb fields = foldM parseField mempty (Map.toList fields)
  where
    parseField accum (fieldName, fieldValues) = do
      case readProgramName "-options" programDb fieldName of
        Nothing -> warnUnknownFields fieldName fieldValues >> return accum
        Just program -> do
          args <- parseProgramArgsField fieldValues
          return $ accum <> (MapMappend $ Map.singleton program args)

-- | Parse fields of a program-locations stanza.
parseProgramPaths :: ProgramDb -> Fields Position -> ParseResult (MapLast String FilePath)
parseProgramPaths programDb fields = foldM parseField mempty (Map.toList fields)
  where
    parseField accum (fieldName, fieldValues) = do
      case readProgramName "-location" programDb fieldName of
        Nothing -> warnUnknownFields fieldName fieldValues >> return accum
        Just program -> do
          fp <- parseProgramPathsField fieldValues
          return $ accum <> (MapLast $ Map.singleton program fp)

-- | Parse all arguments to a single program in program-options stanza.
-- By processing '[NamelessField Position]', we support multiple occurrences of the field, concatenating the arguments.
parseProgramArgsField :: [NamelessField Position] -> ParseResult ([String])
parseProgramArgsField fieldValues =
  concat <$> mapM (\(MkNamelessField pos lines') -> parseProgramArgsFieldLines pos lines') fieldValues

parseProgramPathsField :: [NamelessField Position] -> ParseResult (FilePath)
parseProgramPathsField fieldValues = case fieldValues of
  (MkNamelessField pos lines') : _ -> runFieldParser pos parsecFilePath cabalSpec lines'
  [] -> error "TODO investigate whether this is even possible" -- TODO create a test for field without value "ghc-location: "
  -- example output for cabal-version without a value:
  -- Errors encountered when parsing cabal file ./tmp.cabal:
  -- tmp.cabal:1:1: error:
  -- unexpected end of input
  -- expecting white space, version digit (integral without leading zeroes), opening paren or operator
  --     1 | cabal-version:
  --       | ^
  --   [] -> undefined

-- | Parse all fieldLines of a single field occurrence in a program-options stanza.
parseProgramArgsFieldLines :: Position -> [FieldLine Position] -> ParseResult [String]
parseProgramArgsFieldLines pos = runFieldParser pos programArgsFieldParser cabalSpec

programArgsFieldParser :: CabalParsing m => m [String]
programArgsFieldParser = parseSep (Proxy :: Proxy FSep) parsecToken

type FieldSuffix = String

-- | Extract the program name of a <progname> field, allow it to have a suffix such as '-options' and check whether the 'ProgramDB' contains it.
readProgramName :: FieldSuffix -> ProgramDb -> FieldName -> Maybe String
readProgramName suffix programDb fieldName =
  parseProgramName suffix fieldName >>= ((flip lookupKnownProgram) programDb) >>= pure . programName

parseProgramName :: FieldSuffix -> FieldName -> Maybe String
parseProgramName suffix fieldName = case runParsecParser parser' "<parseProgramName>" fieldNameStream of
  Left err -> trace (show err) Nothing -- TODO should parseWarning Unknown Field fieldName
  Right str -> Just str
  where
    parser' = P.manyTill P.anyChar (P.try ((P.string suffix)) <* P.eof)
    fieldNameStream = fieldLineStreamFromBS fieldName

-- | Issue a 'PWTUnknownField' warning at all occurrences of a field.
warnUnknownFields :: FieldName -> [NamelessField Position] -> ParseResult ()
warnUnknownFields fieldName fieldLines = for_ fieldLines (\field -> parseWarning (pos field) PWTUnknownField message)
  where
    message = "Unknown field: " ++ show fieldName
    pos = namelessFieldAnn

cabalSpec :: CabalSpecVersion
cabalSpec = cabalSpecLatest
