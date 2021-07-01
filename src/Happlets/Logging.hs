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
    -- ^ Report everything, including animation event handlers and mouse drag event
    -- handlers. Reports at this level produce many megabytes of log text in minutes.
  | SIGNAL
   -- ^ Report all incomming low-level signal received, __except for__ animation or mouse drag
   -- events.
  | EVENT
    -- ^ Report high-level events that have been generated after interpreting the signal stream,
    -- only animation and mouse drag events are not printed at this level. This also prints event
    -- delegation calls, so can produce a lot of logging information.
  | OBJECT
    -- ^ Report initialization and destruction of objects (closures) and the activation or
    -- deactivation of event handlers.
  | DEBUG Int
    -- ^ User-defined report levels. Higher levels are more important.
  | INFO
    -- ^ Report Non-essential messages that can be safely ignored. If you have a "--verbose" flag to
    -- your program, set this 'ReportLevel'. Here you can print computational progress information
    -- that might notify a command-line user of what computations are happening, estimated and
    -- actual execution time information, or telemetry data if your app doesn't require very
    -- rigorous or sophisticated telemetry.
  | WARN
    -- ^ The default report level. Report on missing information, or actions that were not taken due
    -- to some runtime condition not being met, or when a default setting overrides a user-requested
    -- setting because user setting was nonsense.
  | ERROR
    -- ^ Highest-priority report level, messages at this level are always printed regardless of the
    -- debug level filter. Caught exceptions should reported be at this level.
  deriving (Eq, Ord, Show)

type LogReporter m = ReportLevel -> Strict.Text -> m ()

-- | Monadic functions that have been called from the 'Happlets.Initialize.Initialize' context will
-- have access to the global logging function. The 'report' function here can be instantiated for
-- any monadic function to provide programmer access to the global logging function.
class Monad m => CanWriteReports m where
  report :: LogReporter m
