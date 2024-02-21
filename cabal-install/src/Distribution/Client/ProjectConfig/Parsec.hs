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

import Network.URI (URI (..), parseURI)

import Control.Monad.State.Strict (StateT, execStateT, lift, modify)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.CabalSpecVersion
import Distribution.Client.HttpUtils
import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Distribution.FieldGrammar
import Distribution.FieldGrammar.Parsec (NamelessField (..), namelessFieldAnn)
import Distribution.Fields.ConfVar (parseConditionConfVarFromClause)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Verbosity

-- TODO #6101 .Legacy -> ProjectConfigSkeleton should probably be moved here
import Distribution.Client.ProjectConfig.FieldGrammar (packageConfigFieldGrammar, projectConfigFieldGrammar)
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigImport, ProjectConfigSkeleton)
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types (MapLast (..), MapMappend (..), PackageConfig (..), ProjectConfig (..), ProjectConfigShared (..))
import Distribution.Client.Types.SourceRepo (SourceRepoList, sourceRepositoryPackageGrammar)
import Distribution.Fields.ConfVar (parseConditionConfVar)
import Distribution.Fields.ParseResult

-- AST type
import Distribution.Fields (Field (..), FieldLine (..), FieldName, Name (..), SectionArg (..), readFields')
import Distribution.Fields.LexerMonad (LexWarning, toPWarnings)
import Distribution.PackageDescription.Quirks (patchQuirks)
import Distribution.Parsec (CabalParsing, ParsecParser, explicitEitherParsec, parsec, parsecFilePath, parsecToken, runParsecParser, simpleParsec, simpleParsecBS)
import Distribution.Parsec.Position (Position (..), zeroPos)
import Distribution.Parsec.Warning (PWarnType (..))
import Distribution.Simple.Program.Db (ProgramDb, defaultProgramDb, knownPrograms, lookupKnownProgram)
import Distribution.Simple.Program.Types (programName)
import Distribution.Simple.Setup (Flag (..))
import Distribution.Types.CondTree (CondBranch (..), CondTree (..), traverseCondTreeC)
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.PackageName (PackageName)
import Distribution.Utils.Generic (breakMaybe, fromUTF8BS, toUTF8BS, unfoldrM, validateUTF8)

import qualified Data.ByteString as BS
import qualified Distribution.Compat.CharParsing as P
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, isPathSeparator, makeValid, takeDirectory, (</>))
import qualified Text.Parsec

-- | Preprocess file and start parsing
parseProjectSkeleton' :: FilePath -> BS.ByteString -> ParseResult ProjectConfigSkeleton
parseProjectSkeleton' source bs = do
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
partitionConditionals :: [[Section ann]] -> ([Section ann], [Conditional ann])
partitionConditionals sections = (concat sections, [])

projectSkeletonImports :: ProjectConfigSkeleton -> [ProjectConfigImport]
projectSkeletonImports = view traverseCondTreeC

singletonProjectConfigSkeleton :: ProjectConfig -> ProjectConfigSkeleton
singletonProjectConfigSkeleton x = CondNode x mempty mempty

readPreprocessFields :: BS.ByteString -> ParseResult [Field Position]
readPreprocessFields bs = do
  case readFields' bs' of
    Right (fs, lexWarnings) -> do
      parseWarnings (toPWarnings lexWarnings)
      for_ invalidUtf8 $ \pos ->
        parseWarning zeroPos PWTUTF $ "UTF8 encoding problem at byte offset " ++ show pos
      return fs
    Left perr -> parseFatalFailure pos (show perr)
      where
        ppos = Text.Parsec.errorPos perr
        pos = Position (Text.Parsec.sourceLine ppos) (Text.Parsec.sourceColumn ppos)
  where
    invalidUtf8 = validateUTF8 bs
    bs' = case invalidUtf8 of
      Nothing -> bs
      Just _ -> toUTF8BS (fromUTF8BS bs)

parseProjectSkeleton :: FilePath -> HttpTransport -> Verbosity -> [ProjectConfigImport] -> FilePath -> BS.ByteString -> IO (ParseResult ProjectConfigSkeleton)
parseProjectSkeleton cacheDir httpTransport verbosity seenImports source bs = (sanityWalkPCS False =<<) <$> liftPR (go []) (readPreprocessFields bs) -- (ParseUtils.readFields bs)
  where
    go :: [Field Position] -> [Field Position] -> IO (ParseResult ProjectConfigSkeleton)
    go acc (x : xs) = case x of
      (Field (Name pos name) importLines) | name == "import" -> do
        liftPR
          ( \importLoc -> do
              if (importLoc `elem` seenImports)
                then pure $ parseFatalFailure pos ("cyclical import of " ++ importLoc)
                else do
                  let fs = fmap (\z -> CondNode z [importLoc] mempty) $ fieldsToConfig (reverse acc)
                  res <- parseProjectSkeleton cacheDir httpTransport verbosity (importLoc : seenImports) importLoc =<< fetchImportConfig importLoc
                  rest <- go [] xs
                  pure . fmap mconcat . sequence $ [fs, res, rest]
          )
          (parseImport pos importLines)
      (Section (Name pos name) args xs') | name == "if" -> do
        subpcs <- go [] xs'
        let fs = fmap singletonProjectConfigSkeleton $ fieldsToConfig (reverse acc)
        (elseClauses, rest) <- parseElseClauses xs
        let condNode =
              (\c pcs e -> CondNode mempty mempty [CondBranch c pcs e])
                <$> parseConditionConfVar args
                <*> subpcs
                <*> elseClauses
        pure . fmap mconcat . sequence $ [fs, condNode, rest]
      _ -> go (x : acc) xs
    go acc [] = pure . fmap singletonProjectConfigSkeleton . fieldsToConfig $ reverse acc

    parseElseClauses :: [Field Position] -> IO (ParseResult (Maybe ProjectConfigSkeleton), ParseResult ProjectConfigSkeleton)
    parseElseClauses x = case x of
      (Section (Name pos name) _args xs' : xs) | name == "else" -> do
        subpcs <- go [] xs'
        rest <- go [] xs
        pure (Just <$> subpcs, rest)
      (Section (Name pos name) args xs' : xs) | name == "elif" -> do
        subpcs <- go [] xs'
        (elseClauses, rest) <- parseElseClauses xs
        let condNode =
              (\c pcs e -> CondNode mempty mempty [CondBranch c pcs e])
                <$> parseConditionConfVar args
                <*> subpcs
                <*> elseClauses
        pure (Just <$> condNode, rest)
      _ -> (\r -> (pure Nothing, r)) <$> go [] x

    -- TODO for multiple lines the legacy parser just took a concatenated version of all lines
    parseImport :: Position -> [FieldLine Position] -> ParseResult (ProjectConfigImport)
    parseImport pos lines = runFieldParser pos (P.many P.anyChar) cabalSpec lines

    -- TODO emit unrecognized field warning on unknown fields, legacy parser does this
    fieldsToConfig :: [Field Position] -> ParseResult ProjectConfig
    fieldsToConfig xs = do
      let (fs, sectionGroups) = partitionFields xs
          (sections, conditionals) = partitionConditionals sectionGroups
          msg = show sectionGroups
      config <- parseFieldGrammar cabalSpec fs (projectConfigFieldGrammar source)
      config' <- view stateConfig <$> execStateT (goSections defaultProgramDb sections) (SectionS config)
      return config'

    fetchImportConfig :: ProjectConfigImport -> IO BS.ByteString
    fetchImportConfig pci = case parseURI pci of
      Just uri -> do
        let fp = cacheDir </> map (\x -> if isPathSeparator x then '_' else x) (makeValid $ show uri)
        createDirectoryIfMissing True cacheDir
        _ <- downloadURI httpTransport verbosity uri fp
        BS.readFile fp
      Nothing ->
        BS.readFile $
          if isAbsolute pci then pci else takeDirectory source </> pci

    modifiesCompiler :: ProjectConfig -> Bool
    modifiesCompiler pc = isSet projectConfigHcFlavor || isSet projectConfigHcPath || isSet projectConfigHcPkg
      where
        isSet f = f (projectConfigShared pc) /= NoFlag

    sanityWalkPCS :: Bool -> ProjectConfigSkeleton -> ParseResult ProjectConfigSkeleton
    sanityWalkPCS underConditional t@(CondNode d _c comps)
      | underConditional && modifiesCompiler d = parseFatalFailure zeroPos "Cannot set compiler in a conditional clause of a cabal project file"
      | otherwise = mapM_ sanityWalkBranch comps >> pure t

    sanityWalkBranch :: CondBranch ConfVar [ProjectConfigImport] ProjectConfig -> ParseResult ()
    sanityWalkBranch (CondBranch _c t f) = traverse (sanityWalkPCS True) f >> sanityWalkPCS True t >> pure ()

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

-- | Monad in which sections are parsed
type SectionParser = StateT SectionS ParseResult

-- | State of 'SectionParser'
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
  | name == "package" = do
      package <- lift $ parsePackageName pos args
      case package of
        Just AllPackages -> do
          pkgCfg <- lift $ parseFieldGrammar cabalSpec fields packageConfigFieldGrammar
          stateConfig . L.projectConfigAllPackages .= pkgCfg
        Just (SpecificPackage packageName) -> do
          pkgCfg <- lift $ parseFieldGrammar cabalSpec fields packageConfigFieldGrammar
          stateConfig . L.projectConfigSpecificPackage %= (\spcs -> spcs <> (MapMappend $ Map.singleton packageName pkgCfg))
        Nothing -> return ()
      unless (null sections) (warnInvalidSubsection pos name)
  | otherwise = do
      warnInvalidSubsection pos name
  where
    (fields, sections) = partitionFields secFields

data PackageConfigTarget = AllPackages | SpecificPackage PackageName

-- TODO what happens when package * is used more than once? maybe emit "warning, not more than once?" what happens atm?
parsePackageName :: Position -> [SectionArg Position] -> ParseResult (Maybe PackageConfigTarget)
parsePackageName pos args = case args of
  [SecArgName _ secName] -> parseName secName
  [SecArgStr _ secName] -> parseName secName
  [SecArgOther _ secName] -> parseName secName
  _ -> do
    parseWarning pos PWTUnknownSection "target package name or * required"
    return Nothing
  where
    parseName secName = case runParsecParser parser "<parsePackageName>" (fieldLineStreamFromBS secName) of
      Left _ -> return Nothing
      Right cfgTarget -> return $ pure cfgTarget
    parser :: ParsecParser PackageConfigTarget
    parser =
      P.choice [P.try (P.char '*' >> return AllPackages), SpecificPackage <$> parsec]

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
parseProgramName suffix fieldName = case runParsecParser parser "<parseProgramName>" fieldNameStream of
  Left _ -> Nothing
  Right str -> Just str
  where
    parser = P.manyTill P.anyChar (P.try ((P.string suffix)) <* P.eof)
    fieldNameStream = fieldLineStreamFromBS fieldName

-- | Issue a 'PWTUnknownField' warning at all occurrences of a field.
warnUnknownFields :: FieldName -> [NamelessField Position] -> ParseResult ()
warnUnknownFields fieldName fieldLines = for_ fieldLines (\field -> parseWarning (pos field) PWTUnknownField message)
  where
    message = "Unknown field: " ++ show fieldName
    pos = namelessFieldAnn

cabalSpec :: CabalSpecVersion
cabalSpec = cabalSpecLatest
