module Happlets.Logging where

import qualified Data.Text as Strict

-- | These are the logging levels that can be set. When calling the 'report' function, you pass a
-- value of this data type along with the message. If the m logging level you 'report' is less than
-- or equal to the current 'Happlets.Initialize.configMaxLogLevel', the message will be
-- reported. 'ERROR's are always reported. Setting 'Happlets.Initialize.configMaxLogLevel' to
-- 'DEBUG' prints all messages.
data ReportLevel = ERROR | WARN | INFO | DEBUG
  deriving (Eq, Ord, Show, Enum)

type LogReporter m = ReportLevel -> Strict.Text -> m ()

-- | Monadic functions that have been called from the 'Happlets.Initialize.Initialize' context will
-- have access to the global logging function. The 'report' function here can be instantiated for
-- any monadic function to provide programmer access to the global logging function.
class Monad m => CanWriteReports m where
  report :: LogReporter m
