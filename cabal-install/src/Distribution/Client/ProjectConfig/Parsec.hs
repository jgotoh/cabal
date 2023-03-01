-- | Parsing project configuration.

module Distribution.Client.ProjectConfig.Parsec (
  -- * Package configuration
  parseProjectSkeleton,

  -- ** Parsing
  ParseResult,
  runParseResult
  ) where

-- TODO #6101 .Legacy -> ProjectConfigSkeleton should probably be moved here
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton)
import Distribution.Fields.ParseResult

import qualified Data.ByteString                                   as BS

parseProjectSkeleton :: BS.ByteString -> ParseResult ProjectConfigSkeleton
parseProjectSkeleton = undefined
