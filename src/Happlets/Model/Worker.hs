module Happlets.Model.Worker
  ( -- ** Thread "Workspaces"
    Workspace, newWorkspace, copyWorkspace,
    WorkerName, recruitWorker, relieveWorker, relieveWorkersIn,
    WorkCycleTime(..),

    -- ** Posts: where assigned workers can do work
    WorkPost, postWorkersUnion, pushWork, checkWork,
    postWorkers, postTaskWorkers, closeWorkPost,
    WorkPostStats(..), getWorkPostStats, resetWorkPostStats, instantThroughput, totalThroughput,
    WorkerTask,  taskPause, thisWorker, taskDone, taskSkip, taskFail, taskCatch,

    -- ** The 'Worker' abstraction
    Worker, workerThreadId, workerName,
    WorkerStatus(..), getWorkerStatus, workerNotBusy, workerNotDone, workerBusy, workerFailed,

    -- ** Unions: Groups of Worker Threads
    WorkerUnion, newWorkerUnion, unionizeWorker, retireWorker,

    -- ** Worker Providers
    WorkerSignal(..), WorkerNotification(..), setWorkerStatus, runWorkerTask,
    WorkerID, workerIDtoInt,

  ) where

import           Happlets.Model.GUI

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Time.Clock
import           Data.List             (intercalate)
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Data.Text             as Strict

import           System.IO.Unsafe      (unsafePerformIO)

----------------------------------------------------------------------------------------------------

modifyMVar' :: MVar st -> (st -> IO (a, st)) -> IO a
modifyMVar' mvar f = modifyMVar mvar $ fmap (\ (a, st) -> (st, a)) . f

-- $Workers
-- Multi-threaded Happlets made easy.
--
-- All worker related functions evaluate to a function of type: @forall m a . 'MonadIO' m => m a@.
-- This means it is possible to use any of these functions to create and manage 'Worker's and
-- 'WorkerUnions' in @IO@, 'Initialize', 'GUI', and the worker-specific 'WorkerTask' function type.
--
-- The Happlets library also defines the concept of a "government", which are 'Worker's that are
-- members of the default 'WorkerUnion' provided by the 'GUI' function'. All Happlet
-- 'Happlet.Provider.Provider's must provide a government 'WorkerUnion' as an element of the
-- 'GUIState'. To recruit government 'Worker's, evaluate 'guiWorker' or 'govWorker'.

-- Is either 'Control.Concurrent.Yield' or @return ()@, whichever one has the expected behavior.
blink :: IO ()
blink = yield

-- | Obtain a 'WorkerStatus' using the 'getWorkerStatus' function. 
data WorkerStatus
  = WorkerInit      -- ^ is getting started
  | WorkerWaiting   -- ^ is waiting for a lock to open
  | WorkerPaused    -- ^ is waiting for a timer to go off
  | WorkerBusy      -- ^ is working normally
  | WorkerHalted    -- ^ evaluated to 'taskDone'
  | WorkerFlagged   -- ^ signaled with 'relieveWorker'
  | WorkerFailed  Strict.Text -- ^ worker evaluated to 'fail'
  deriving (Eq, Show)

-- | Is the 'WorkerStatus' set to 'WorkerInit', 'WorkerWaiting', or 'WorkerPaused'?
workerNotBusy :: WorkerStatus -> Bool
workerNotBusy = \ case
  WorkerInit    -> True
  WorkerWaiting -> True
  WorkerPaused  -> True
  _             -> False

-- | Is the 'WorkerStatus' set to 'WorkerCompleted', 'WorkerCanceled', 'WorkerHalted',
-- 'WorkerFlaggeed', or 'WorkerFailed'?
workerNotDone :: WorkerStatus -> Bool
workerNotDone = \ case
  WorkerHalted    -> True
  WorkerFlagged   -> True
  WorkerFailed{}  -> True
  _               -> False

-- | Is the 'WorkerStatus' set to 'WorkerBusy'?
workerBusy :: WorkerStatus -> Bool
workerBusy = \ case { WorkerBusy -> True; _ -> False; }

-- | Is the 'WorkerStatus' set to 'WorkerFailed'? If so, get the error message.
workerFailed :: WorkerStatus -> Maybe Strict.Text
workerFailed = \ case { WorkerFailed msg -> Just msg; _ -> Nothing; }

-- | This is just an arbitrary string you can use to identify 'Workers'. Nothing prevents you from
-- giving two different workers the same handle.
type WorkerName = Strict.Text

-- | 'Worker's always evaluate in loops. When recruiting a worker using 'recruitWorker',
-- 'guiWorker', or 'govWorker', specify this value to limit the rate of work being done. This can be
-- useful when you need slow but periodic updates. One example would be for a Happlet that shows a
-- clock which updates every second, for this you would pass @('WorkCycle' 1.0)@ to the 'guiWorker'
-- function.
data WorkCycleTime
  = WorkCycleASAP
    -- ^ Indicates that the worker thread should never delay, each next cycle should begin as soon
    -- as the previous cycle ends. Usually this would be used when spawining workers to do intensive
    -- computations on a large set of data in the backgroud.
  | WorkCycleWait !Double
    -- ^ indicates that the 'Worker' should rest the given number of seconds between each cycle.
  | WorkWaitCycle !Double
    -- ^ Like 'WorkCycle' but rests before work begins, rather than after each cycle ends. The
    -- difference is whether the first cycle occurs immediately or not after a worker is recruited.
  deriving (Eq, Ord, Show)

-- | This data structure contains information about an individual worker. It contains a 'workerName'
-- which can contain an arbitrary string you can use to identify them, although nothing prevents you
-- from giving two different workers the same name. The truly unique identifier is 'workerThreadId'
-- which is assigned to each worker by the Haskell runtime system and is always unique throughout
-- the duration of a program's process lifetime.
--
-- This data structure is only an identity for an actual worker thread. Once the 'WorkerTask' is
-- done, the worker leaves the 'WorkerUnion' and will never return under the same identity.
data Worker
  = Worker
    { workerThreadId :: !WorkerID
    , workerHalt     :: !(IO ())
    , workerName     :: !WorkerName
    , workerStatus   :: !(MVar WorkerStatus)
    }

instance Eq   Worker where { a == b = workerThreadId a == workerThreadId b; }
instance Ord  Worker where
  compare a b = compare (workerThreadId a) (workerThreadId b) <>
    compare (workerName a) (workerName b)
instance Show Worker where
  show  worker = '(':showWorker worker++")"
  showList lst = ('[' :) . ((intercalate "," (showWorker <$> lst)) ++) . (']' :)

newtype WorkerID = WorkerID{ workerIDtoInt :: Int } deriving (Eq, Ord, Show)

_workerIDMVar :: MVar Int
_workerIDMVar = unsafePerformIO $ newMVar 0
{-# NOINLINE _workerIDMVar #-}

newWorkerID :: IO WorkerID
newWorkerID = modifyMVar _workerIDMVar $ \ a -> return (a+1, WorkerID $ a+1)

showWorker :: Worker -> String
showWorker (Worker{workerThreadId=tid,workerName=name}) = show tid++": "++Strict.unpack name

-- | The 'GUI' provides access to a default 'WorkerUnion' referred to as the "government" union. You
-- can easily recruit new workers using 'govWorker'. Government workers are equal to any other
-- worker, so you can relieve them using 'relieveWorker' function.
--
-- It is also possible to create local 'WorkerUnion's using 'newWorkerUnion', and manage work using
-- 'recruitWorker' and 'relieveWorker'.
newtype WorkerUnion = WorkerUnion (MVar (Set.Set Worker))

-- | A 'WorkerSignal' is a message sent to a worker when you need them to quit the task they are
-- working on. Use 'relieveWorkersIn' or 'relieveGovWorkers' to inspect the 'Worker' of each worker
-- and send them a signal of this data type.
newtype WorkerSignal = WorkerSignal () deriving (Eq, Ord, Show)
instance Exception WorkerSignal

-- | A 'Workspace' contains @work@ piece for a worker to work on. It is a thread-safe mutex
-- variable.
newtype Workspace work = Workspace{ unwrapWorkspace :: MVar work }

-- | This is the worker thread type. Workers loop continuously by default, you can cancel any time
-- by evaluating 'taskDone', you can cancel the current work item by evaluating 'taskSkip', you can
-- report a failure with 'taskFail'.
--
-- Notice the 'WorkerTask' type takes a @work@ type, this is the contents of a 'Workspace'.
newtype WorkerTask work a
  = WorkerTask { unwrapWorkerTask :: ReaderT Worker (StateT work IO) (Consequence a) }

-- | Evaluae a 'WorkerTask' using a given 'Worker' by locking an 'Control.Concurrent.MVar.MVar' and
-- evaluating the 'WorkerTask' function to work on the content of this
-- 'Control.Concurrent.MVar.MVar'.
runWorkerTask :: Worker -> WorkerTask work a -> MVar work -> IO (Consequence a)
runWorkerTask worker (WorkerTask f) mvar = modifyMVar' mvar $ runStateT (runReaderT f worker)

-- | A 'WorkPost' is an area where 'Worker's waits to recieve work. Send work to the post using the
-- 'postWork' function, and check on the result using 'postCheck' function. 'Worker's are assigned
-- to a post using 'postWorkers' or 'guiPostWorkers'. 'Worker's working in a 'WorkPost' will form
-- their own 'WorkerUnion', rather than being assigned to some other 'WorkerUnion' or the government
-- 'WorkerUnion'.
--
-- Be warned that posting too much work faster than the 'Worker's can handle it will result in a
-- "space leak", a situation in which unprocessed work piles up taking more and more memory until
-- your program is forcibly halted by the operating system.
--
-- You can check throughput statistics on a 'WorkPost' as well.
data WorkPost inWork outWork
  = WorkPost
    { postWorkersUnion :: !WorkerUnion
    , workPostStats    :: !(MVar WorkPostStats)
    , workPostInSema   :: !QSem -- inbox semaphore
    , workPostInbox    :: !(MVar (Seq.Seq inWork))
    , workPostOutbox   :: !(MVar (Seq.Seq outWork))
    }

-- | Statistics on a 'WorkPost'.
data WorkPostStats
  = WorkPostStats
    { workPostTotalOutput   :: !Int
      -- ^ The total number of elements produced. This value is never reset
    , workPostTotalInput    :: !Int
      -- ^ The total number of elements received. This value is never reset.
    , workPostRecentOutput  :: !Int
      -- ^ The number of elements produced since these statistics were last reset by
      -- 'resetWorkPostStats'.
    , workPostRecentInput   :: !Int
      -- ^ The number of elements recieved since these statistics were last reset by
      -- 'resetWorkPostStats'.
    , workPostWaitingOutput :: !Int
      -- ^ The number of completed items not yet retrieved.
    , workPostWaitingInput  :: !Int
      -- ^ The number of pushed items not yet processed.
    , workPostStartTime     :: !UTCTime
      -- ^ The time this post was created. This value is never reset.
    , workPostResetTime     :: !UTCTime
      -- ^ The time at which this statistics report was last reset with 'resetWorkPostStats'.
    , workPostCheckTime     :: !UTCTime
      -- ^ The time at which this statistics report was created.
    }

instance Functor (WorkerTask work) where
  fmap f (WorkerTask m) = WorkerTask $ fmap (fmap f) m

instance Applicative (WorkerTask work) where { pure = return; (<*>) = ap; }

instance Monad (WorkerTask work) where
  return = WorkerTask . return . ActionOK
  (WorkerTask a) >>= f = WorkerTask $ a >>= \ case
    ActionOK   a   -> unwrapWorkerTask $ f a
    ActionHalt     -> return ActionHalt
    ActionCancel   -> return ActionCancel
    ActionFail msg -> return $ ActionFail msg

instance MonadFail (WorkerTask work) where
  fail   = taskFail . Strict.pack

instance MonadState work (WorkerTask work) where
  state = WorkerTask . fmap ActionOK . lift . state

-- | When a 'WorkerTask' is being evaluated, a 'Worker' may refer to one's self by obtaining a copy
-- of it's identification (a union membership card).
thisWorker :: WorkerTask work Worker
thisWorker = WorkerTask $ ActionOK <$> ask

-- | This function obtains status information from a given 'Worker'. The worker may update it's
-- own status at any time, so the returned 'WorkerStatus' is only a snapshot of the status at a
-- given time, it may even change the very moment after this function returns.
getWorkerStatus :: MonadIO m => Worker -> m WorkerStatus
getWorkerStatus = liftIO . readMVar . workerStatus

-- | The task should stop looping and close-up shop.
taskDone :: WorkerTask work void
taskDone = WorkerTask $ return ActionCancel

-- | The task should skip this loop and wait for the next loop.
taskSkip :: WorkerTask work void
taskSkip = WorkerTask $ return ActionHalt

-- | Signal a failure occured.
taskFail :: Strict.Text -> WorkerTask work void
taskFail = WorkerTask . return . ActionFail

-- | Catch a 'taskSkip' or 'taskFail' signal, or really any signal at all.
taskCatch :: WorkerTask work a -> (Consequence a -> WorkerTask work b) -> WorkerTask work b
taskCatch (WorkerTask f) catch = WorkerTask $ f >>= unwrapWorkerTask . catch

-- | This function is used by Happlet 'Happlets.Initialize.Provider's when creating 'Worker'
-- threads, namely in instances of 'launchWorkerThread', to notify the outside world of the status
-- of the 'Worker' as it is evaluated in the parallel thread.
setWorkerStatus :: MonadIO m => Worker -> WorkerStatus -> m WorkerStatus
setWorkerStatus (Worker{workerStatus=mvar}) = liftIO . swapMVar mvar

-- | Pause for some number of seconds. The pause time can be very small, but the smallest pause time
-- is dependent on the operating system and computer hardware. Haskell's runtime system on stock
-- computer hardware can control time to within a millionth of a second with some reliability.
taskPause :: Double -> WorkerTask work ()
taskPause t = WorkerTask $ ReaderT $ \ self -> liftIO $ do
  oldStat <- setWorkerStatus self WorkerPaused
  if t <= 0 then blink else threadDelay $ round $ t * 1000 * 1000
  setWorkerStatus self oldStat
  return $ ActionOK ()

-- | Create a new 'Workspace' for a worker to work on, containing a initial @work@ piece.
newWorkspace :: MonadIO m => work -> m (Workspace work)
newWorkspace = liftIO . fmap Workspace . newMVar

-- | Get a copy of the @work@ piece from the 'Workspace' without disturbing the current
-- 'WorkerTask'. The copy is just a snapshot, unless the 'Worker' has completed it's 'WorkerTask',
-- this snapshot will likely go out of date the moment this function call returns.
copyWorkspace :: MonadIO m => Workspace work -> m work
copyWorkspace = liftIO . readMVar . unwrapWorkspace

-- | Happlet 'Happlet.Initialize.Provider's should instantiate the 'launchWorkerThread' function
-- such that the 'Worker' threads correctly report on their status. Use the functions provided by
-- this data structure to notify whether they are waiting, busy, halted, or failed.
newtype WorkerNotification = WorkerNotification (WorkerStatus -> IO ())

-- | This function starts a worker on a 'WorkerTask', returning an identifier you can use to query
-- information about the 'Worker's status. In this idylic little world, workers stay with a
-- 'WorkerUnion' until they retire, so there are no APIs for moving a 'Worker' from one
-- 'WorkerUnion' to another, you can only signal to them that their work is done using the
-- 'relieveWorker' function, at which point they happily leave.
recruitWorker
  :: MonadIO m
  => WorkerUnion -> Workspace work
  -> WorkerName -> WorkCycleTime -> WorkerTask work void -> m Worker
recruitWorker = _recruitWorker defaultLaunchWorkerThread

_recruitWorker
  :: MonadIO m
  => ForkWorkerThread void
  -> WorkerUnion -> Workspace work
  -> WorkerName -> WorkCycleTime -> WorkerTask work void -> m Worker
_recruitWorker fork wu (Workspace mvar) handl cycle (WorkerTask f) =
  newWorker fork wu handl cycle $ \ worker -> do
    setWorkerStatus worker WorkerWaiting
    modifyMVar' mvar $ \ work -> do
      setWorkerStatus worker WorkerBusy
      runStateT (runReaderT f worker) work

---- | Similar to 'recruitWorker' but works for the "government" and operates on the current @model@
---- in of the 'GUI' function which recruits this 'Worker'. This function is also similar to
---- 'govWorker', but notice that the task to be performed is not an ordinary 'WorkerTask' function,
---- but a 'GUI' function. This allows the worker to update the GUI directly, as if there were an
---- event handler constantly responding to a stream of events being generated in a parallel process.
----
---- The 'Happlet.Provider.Provider' must instantiate 'CanRecruitWorkers' in order for this function
---- to be usable -- we hope that all 'Happlet.Provider.Provider's will do this.
--guiWorker
--  :: CanRecruitWorkers provider m
--  => WorkerName -> WorkCycleTime
--  -> GUI provider model void -> GUI provider model Worker
--guiWorker handl cycle f = do
--  guist <- getGUIState
--  newWorker (launchWorkerThread guist) (theGUIWorkers guist) handl cycle $ \ worker -> do
--    setWorkerStatus worker WorkerWaiting
--    inOtherThreadRunGUI guist (liftIO (setWorkerStatus worker WorkerBusy) >> f)

---- | Similar to 'recruitWorker' but works in the "government" 'WorkerUnion'.
----
---- Notice that this function must be evaluate to a 'GUI' function, unlike many of the other
---- 'Worker'-related functions here which can be evaluated to any function type of the 'MonadIO' type
---- class. The default "government" 'WorkerUnion' is only available during 'GUI' evaluation.
--govWorker
--  :: CanRecruitWorkers provider m
--  => Workspace work -> WorkerName -> WorkCycleTime
--  -> WorkerTask work void -> GUI provider model Worker
--govWorker workspace handl cycle f = do
--  st  <- getGUIState
--  gov <- govWorkerUnion
--  _recruitWorker (launchWorkerThread st) gov workspace handl cycle f

-- | Relieve every worker in the 'postWorkersUnion' using the 'relieveWorkersIn' function, where the
-- filter given to 'relieveWorkersIn' is @('const' 'True')@, resulting in all workers being
-- relieved.
closeWorkPost :: MonadIO m => WorkPost inWork outWork -> m ()
closeWorkPost = flip relieveWorkersIn (const True) . postWorkersUnion

-- | Get a statistical snapshot of the current state of a given 'WorkPost'. With this you can
-- evaluate 'totalThroughput' or 'instantThroughput', or compute your own statistics.
getWorkPostStats :: MonadIO m => WorkPost inWork outWork -> m WorkPostStats
getWorkPostStats = liftIO . readMVar . workPostStats

-- | Like 'getWorkPostStats', but after this function returns, some of the 'WorkPostStats' will be
-- reset, namely the 'workPostRecentInput', 'workPostRecentOutput', and 'workPostResetTime' fields.
resetWorkPostStats :: MonadIO m => WorkPost inWork outWork -> m WorkPostStats
resetWorkPostStats post = liftIO $ do
  t <- getCurrentTime
  modifyMVar (workPostStats post) $ \ st -> return $ flip (,) st $ st
    & (_workPostRecentInput  .~ 0)
    & (_workPostRecentOutput .~ 0)
    & (_workPostResetTime    .~ t)

-- | Construct a group of 'WorkPost' workers, each running the same 'WorkerTask'. Pass a list of
-- 'workerName's, one 'Worker' will be created for each name, all workers will wait on the same
-- 'WorkPost'. The 'closeWorkPost' function does this and halts all workers, at which point the
-- 'WorkPost' can no longer be used.
--
-- Notice the type of the 'WorkerTask': it takes an @inWork@ type, is stateful over the @()@ type
-- (is stateless), and returns an @outWork@ type. This means to yield output, this function merely
-- needs to return a value. The 'WorkerTask' can still evaluate 'taskSkip' to avoid yielding a
-- value.
postWorkers
  :: MonadIO m
  => [WorkerName] -> (inWork -> WorkerTask () outWork)
  -> m (WorkPost inWork outWork)
postWorkers names f =
  postal defaultLaunchWorkerThread names $ \ self work ->
  evalStateT (runReaderT (unwrapWorkerTask $ f work) self) ()

-- | Similar to 'postWorkers' but also allows the workers access to a 'Workspace'. The content of
-- 'Workspace' can be thought of as a @fold@ed value that may be updated after each @inWork@ element
-- is recieved.
postTaskWorkers
  :: MonadIO m
  => Workspace fold -> [WorkerName] -> (inWork -> WorkerTask fold outWork)
  -> m (WorkPost inWork outWork)
postTaskWorkers (Workspace mvar) handls f =
  postal defaultLaunchWorkerThread handls $ \ self work -> do
    setWorkerStatus self WorkerWaiting
    modifyMVar' mvar $ \ fold -> do
      setWorkerStatus self WorkerBusy
      runStateT (runReaderT (unwrapWorkerTask $ f work) self) fold

---- | Similar to 'postWorkers', but the task evaluated is of type 'GUI', allowing full access to the
---- 'GUI's state as each work item is posted.
--guiPostWorkers
--  :: CanRecruitWorkers provider m
--  => [WorkerName] -> (inWork -> GUI provider model outWork)
--  -> GUI provider model (WorkPost inWork outWork)
--guiPostWorkers names f = getGUIState >>= \ st ->
--  postal (launchWorkerThread st) names $ \ self work -> do
--    setWorkerStatus self WorkerWaiting
--    inOtherThreadRunGUI st (setWorkerStatus self WorkerBusy >> f work)

-- not for export
postal
  :: MonadIO m
  => ForkWorkerThread outWork
  -> [WorkerName]
  -> (Worker -> inWork -> IO (Consequence outWork))
  -> m (WorkPost inWork outWork)
postal launch names f = liftIO $ do
  stats  <- newWorkPostStats >>= newMVar
  insema <- newQSem 0
  inbox  <- newMVar Seq.empty
  outbox <- newMVar Seq.empty
  unmvar <- newMVar Set.empty
  let un   = WorkerUnion unmvar
  let post = WorkPost
        { postWorkersUnion = WorkerUnion unmvar
        , workPostStats    = stats
        , workPostInSema   = insema
        , workPostInbox    = inbox
        , workPostOutbox   = outbox
        }
  let make handl = newWorker launch un handl WorkCycleASAP $ \ self -> do
        setWorkerStatus self WorkerWaiting
        waitQSem insema
        setWorkerStatus self WorkerBusy
        work <- modifyMVar inbox $ \ worker -> return $ case Seq.viewl worker of
          a Seq.:< ax -> (ax, a)
          Seq.EmptyL  -> (,) Seq.empty $ error $ "Worker "++show handl++
            " discovered PostWork semaphore signalled without"++
            " having first inserted an element in the inbox."
        postStats post
          $ (_workPostWaitingInput %~ subtract 1)
          . (_workPostTotalInput   %~ (+ 1))
          . (_workPostRecentInput  %~ (+ 1))
        result <- f self work >>= evaluate
        case result of
          ActionOK result -> seq result $! do
            modifyMVar_ outbox $ pure . (Seq.|> result)
            postStats post
              $ (_workPostWaitingOutput %~ (+ 1))
              . (_workPostTotalOutput   %~ (+ 1))
              . (_workPostRecentOutput  %~ (+ 1))
          _                           -> return ()
        return result
  Set.fromList <$> mapM make names >>= swapMVar unmvar
  return post

-- | This is a type of function used to fork a thread for a 'Worker'.
type ForkWorkerThread a
  =  WorkerUnion
      -- ^ use to call 'unionizeWorker' when thread begins and 'retireWorker' when thread ends.
  -> WorkCycleTime -- ^ requests the cycle time of the work thread that is forked here.
  -> IO Worker     -- ^ used to obtain a copy of the 'Worker' which represents thread forked here.
  -> (Worker -> IO (Consequence a))
     -- ^ the task to be perofmred during each work cycle.
  -> IO (IO ())    -- ^ returns a function that can be used to halt the thread that is forked here.

-- not for export
newWorker
  :: MonadIO m
  => ForkWorkerThread void
  -> WorkerUnion -> WorkerName -> WorkCycleTime
  -> (Worker -> IO (Consequence void))
  -> m Worker
newWorker fork wu handl cycle f = liftIO $ do
  stat <- newMVar WorkerInit
  wid  <- newWorkerID
  mvar <- newMVar Worker
    { workerThreadId = wid
    , workerName     = handl
    , workerStatus   = stat
    , workerHalt     = return ()
    }
  halt <- fork wu cycle (readMVar mvar) f
  modifyMVar mvar $ \ worker' -> do
    let worker = worker'{ workerHalt = halt }
    return (worker, worker)

-- | This function can be used as a default instance of 'launchWorkerThread'. This function makes
-- use of Haskell's own runtime threading system, namely 'Control.Concurrent.forkOS' to launch a
-- thread. Not all 'Happlet.Initialize.Provider's should use this function, however, especially
-- those which cannot provide thread safety when evaluating functions of type @GUI@.
defaultLaunchWorkerThread
  :: WorkerUnion
  -> WorkCycleTime
  -> IO Worker
  -> (Worker -> IO (Consequence void))
  -> IO (IO ())
defaultLaunchWorkerThread wu cycle getWorker f = do
  let delay t = threadDelay $ round $ t * 1000 * 1000
  let workerLoop self = do
        let loop   = setWorkerStatus self WorkerPaused >>
              (case cycle of { WorkCycleWait t -> delay t; _ -> blink; }) >>
              workerLoop self
        result <- f self
        case result of
          ActionOK{}     -> loop
          ActionHalt     -> loop
          ActionCancel   -> void $ setWorkerStatus self WorkerHalted
          ActionFail msg -> void $ setWorkerStatus self $ WorkerFailed msg
  tid <- forkOS $ catches
    (do self <- getWorker
        bracket_ (unionizeWorker wu self) (retireWorker wu self) $ do
          case cycle of
            WorkWaitCycle t -> setWorkerStatus self WorkerPaused >> delay t
            _               -> return ()
          workerLoop self
    )
    [ Handler $ \ (WorkerSignal ()) -> do
        self <- getWorker
        void $ setWorkerStatus self WorkerFlagged
    , Handler $ \ (SomeException e) -> do
        self <- getWorker
        setWorkerStatus self $ WorkerFailed $ Strict.pack $ show e
        throw e
    ]
  return $ throwTo tid $ WorkerSignal ()

-- | Signal a worker that they no longer need to perform their task. In this idylic little world, a
-- worker happily leaves when there is no more work to do, and they abandon their 'Worker' identity,
-- which is their membership in a 'WorkerUnion'. Calling 'relieveWorker' on the same 'Worker' ID
-- that is already retired does nothing.
relieveWorker :: MonadIO m => Worker -> m ()
relieveWorker = liftIO . workerHalt

-- | Relieve members in a given 'WorkerUnion' which match a given predicate.
relieveWorkersIn :: MonadIO m => WorkerUnion -> (Worker -> Bool) -> m ()
relieveWorkersIn (WorkerUnion mvar) toBeRelieved = liftIO $ modifyMVar_ mvar $ \ set -> do
  let (toBe, notToBe) = Set.partition toBeRelieved set
  mapM_ relieveWorker $ Set.toList toBe
  return notToBe

---- | Like 'relieveWorkersIn' but the signals are sent to the workers default "government"
---- 'WorkerUnion'.
--relieveGovWorkers :: (Worker -> Bool) -> GUI provider model ()
--relieveGovWorkers toBeRelieved = govWorkerUnion >>= liftIO . flip relieveWorkersIn toBeRelieved

-- | Define a new 'WorkerUnion'. 
newWorkerUnion :: MonadIO m => m WorkerUnion
newWorkerUnion = liftIO $ WorkerUnion <$> newMVar Set.empty

-- | This function must only be called by Happlet 'Happlets.Initialize.Provider's when instantiating
-- the 'launchWorkerThread' function.
unionizeWorker :: WorkerUnion -> Worker -> IO ()
unionizeWorker (WorkerUnion mvar) worker = liftIO $
  modifyMVar_ mvar $ return . Set.insert worker

-- | This function must only be called by Happlet 'Happlets.Initialize.Provider's when instantiating
-- the 'launchWorkerThread' function.
retireWorker :: WorkerUnion -> Worker -> IO ()
retireWorker (WorkerUnion mvar) worker = liftIO $
  modifyMVar_ mvar $ return . Set.delete worker

-- not for export
postStats :: WorkPost inWork outWork -> (WorkPostStats -> WorkPostStats) -> IO ()
postStats (WorkPost{workPostStats=mvar}) = modifyMVar_ mvar . (return .)

-- | Place @work@ into the 'WorkPost' for it to be processed by 'Worker's. This function is
-- asyncrhonouse.
--
-- Be warned that posting too much work faster than the 'Worker's can handle it will
-- result in a "space leak", a situation in which unprocessed work piles up taking more and more
-- memory until your program is forcibly halted by the operating system.
pushWork :: MonadIO m => WorkPost inWork outWork -> inWork -> m ()
pushWork post work = liftIO $ do
  modifyMVar_ (workPostInbox post) $ pure . (Seq.|> work)
  postStats post $ _workPostWaitingInput %~ (+ 1)
  signalQSem $ workPostInSema post

-- | Check if @work@ being processed in a 'WorkPost' has completed. This function is
-- asynchronous. If there is no @work@ completed yet, 'Prelude.Nothing' is returned.
checkWork :: MonadIO m => WorkPost inWork outWork -> m (Maybe outWork)
checkWork post = liftIO $ do
  a <- modifyMVar (workPostOutbox post) $ \ workers -> return $ case Seq.viewl workers of
    Seq.EmptyL  -> (Seq.empty, Nothing)
    a Seq.:< ax -> (ax, Just a)
  case a of
    Nothing -> return ()
    Just{}  -> postStats post $ _workPostWaitingOutput %~ subtract 1
  return a

-- | 'WorkPost' instantaneous throughput measured in elements per second. "Instantaneous" means the
-- number of elements processed (taken from the output by 'checkPost') since the last time the
-- 'WorkPost' was reset with 'resetWorkPostStats'.
instantThroughput :: WorkPostStats -> Double
instantThroughput stats = realToFrac (workPostRecentOutput stats) /
  realToFrac (workPostCheckTime stats `diffUTCTime` workPostResetTime stats)

-- | 'WorkPost' total throughput measured in elements per second. "Total" means the number of
-- elements processed (taken from the output by 'checkPost') since the 'WorkPost' was created.
totalThroughput :: WorkPostStats -> Double
totalThroughput stats = realToFrac (workPostTotalOutput stats) /
  realToFrac (workPostCheckTime stats `diffUTCTime` workPostStartTime stats)

_workPostTotalOutput   :: Lens' WorkPostStats Int
_workPostTotalOutput   = lens workPostTotalOutput $ \ a b -> a{ workPostTotalOutput = b }

_workPostTotalInput    :: Lens' WorkPostStats Int
_workPostTotalInput    = lens workPostTotalInput $ \ a b -> a{ workPostTotalInput = b }

_workPostRecentOutput  :: Lens' WorkPostStats Int
_workPostRecentOutput  = lens workPostRecentOutput $ \ a b -> a{ workPostRecentOutput = b }

_workPostRecentInput   :: Lens' WorkPostStats Int
_workPostRecentInput   = lens workPostRecentInput $ \ a b -> a{ workPostRecentInput = b }

_workPostWaitingOutput :: Lens' WorkPostStats Int
_workPostWaitingOutput = lens workPostWaitingOutput $ \ a b -> a{ workPostWaitingOutput = b }

_workPostWaitingInput  :: Lens' WorkPostStats Int
_workPostWaitingInput  = lens workPostWaitingInput $ \ a b -> a{ workPostWaitingInput = b }

_workPostStartTime     :: Lens' WorkPostStats UTCTime
_workPostStartTime     = lens workPostStartTime $ \ a b -> a{ workPostStartTime = b }

_workPostResetTime     :: Lens' WorkPostStats UTCTime
_workPostResetTime     = lens workPostResetTime $ \ a b -> a{ workPostResetTime = b }

_workPostCheckTime     :: Lens' WorkPostStats UTCTime
_workPostCheckTime     = lens workPostCheckTime $ \ a b -> a{ workPostCheckTime = b }

newWorkPostStats :: IO WorkPostStats
newWorkPostStats = do
  t <- getCurrentTime
  return WorkPostStats
    { workPostTotalOutput   = 0
    , workPostTotalInput    = 0
    , workPostRecentOutput  = 0
    , workPostRecentInput   = 0
    , workPostWaitingOutput = 0
    , workPostWaitingInput  = 0
    , workPostStartTime     = t
    , workPostResetTime     = t
    , workPostCheckTime     = t
    }
