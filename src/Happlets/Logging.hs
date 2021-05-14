module Happlets.Logging where

import qualified Data.Text as Strict

-- | TODO: move this into the GUI module.
data ReportLevel = DEBUG | ERROR | WARN | INFO
  deriving (Eq, Ord, Show, Enum)

type LogReporter m = ReportLevel -> Strict.Text -> m ()

-- | TODO: move this into the GUI module.
class Monad m => CanWriteReports m where
  report :: LogReporter m
