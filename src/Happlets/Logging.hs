module Happlets.Logging where

import qualified Data.Text as Strict

-- | These are the logging levels that can be set. When calling the 'report' function, you pass a
-- value of this data type along with the message. If the m logging level you 'report' is less than
-- or equal to the current 'Happlets.Initialize.configMaxLogLevel', the message will be
-- reported. 'ERROR's are always reported since they are the highest priority. Setting
-- 'Happlets.Initialize.configMaxLogLevel' to 'DEBUG_ALL' prints all messages.
--
-- The 'DEBUG' level takes an arbitrary 'Int' parameter, it is useful for providing debugging
-- messages, and you can specify whatever priority semantics you choose using the 'Int' parameter,
-- as long as higher 'Int' values indicate higher priority messages.
data ReportLevel
  = DEBUG_ALL
    -- ^ Print all events, including animation event handlers and mouse drag event handlers. Reports
    -- at this level produce many megabytes of log text in minutes.
  | EVENT
    -- ^ Most event signals that come in are reported, only animation and mouse drag events are not
    -- printed at this level.
  | OBJECT
    -- ^ Reporting of the initialization and destruction of objects (closures) and changing event
    -- handlers go at this level.
  | DEBUG Int -- ^ User-defined report levels. Lower levels are more important.
  | INFO  -- ^ Non-essential reports that can be safely ignored.
  | WARN  -- ^ The default report level.
  | ERROR
    -- ^ Highest-priority report level, messages are always printed. Caught exceptions should
    -- reported be at this level.
  deriving (Eq, Ord, Show)

type LogReporter m = ReportLevel -> Strict.Text -> m ()

-- | Monadic functions that have been called from the 'Happlets.Initialize.Initialize' context will
-- have access to the global logging function. The 'report' function here can be instantiated for
-- any monadic function to provide programmer access to the global logging function.
class Monad m => CanWriteReports m where
  report :: LogReporter m
