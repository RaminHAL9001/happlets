-- | This module defines the applet data type 'Happlet', and a function type 'GUI'. These two types
-- define what a Happlet is. When writting a Happlet program, you first define the @model@ data type
-- that your 'Happlet' will view and control, and then you define a variety of GUI functions which
-- manipulate the @model@ and then redraw the window in response to events. The 'GUI' function type
-- is the Happlets equivalent of the @IO@ function type.
--
-- The @model@ is stored into the 'Happlet' and can then be attached to a window using the
-- 'Happlets.Initialize.windowAttach' function in the "Happlets.Initialize" module. When attaching a
-- 'Happlet' to a window, it is also necessary to provide an initial 'GUI' function which will be
-- used to install other 'GUI' functions to be used as event handlers.
--
-- The "Happlets.Control" module provides a consistent API for all back-end
-- 'Happlets.Provider.Provider's, but the subset of which event data types that your 'Happlet'
-- program will be able to respond to depends on the back-end "Happlets.Provider" you are using and
-- which type classes the @provider@ type instantiates. The type classes that provide the API
-- functions for installing 'GUI' event handlers include:
--
--  These type classes provide the functions that allow you to install 'GUI' functions as event
--  handlers into your 'Happlet' when the 'Happlet' is attached to the @window@ using the
--  'Happlets.Initialize.windowAttach' function. You could also think of this as the functions which
--  allow your 'Happlet' to subscribe to certain event channels.
--
-- Once you have created a window and attached a happlet to it (see
-- 'Happlets.Initialize.windowAttach' in the "Happlets.Initialize" module), and end-users use the
-- 'Happlet', events come into your 'Happlet' program triggering the 'GUI' functions which can alter
-- the @model@ data stored in the 'Happlet' using the "Control.Monad.State.Class" functions
-- 'Control.Monad.State.Class.get', 'Control.Monad.State.Class.put',
-- 'Control.Monad.State.Class.modify', and the 'Control.Monad.State.Class.state' functions. However
-- the 'getModel', 'putModel', and 'modifyModel' functions are also provided in this module which
-- are 'GUI'-specific versions of the "Contorol.Monad.State.Class" functions.
--
-- GUI functions update the window using the 'onCanvas' function which evaluates a screen updating
-- function specific to the back-end 'Happlets.Provider.Provider' as defined by the 'HappletWindow'
-- type class.
module Happlets.Model.GUI
  ( -- * The Happlet Data Type
    Happlet, getWaitingThreads,

    -- * The GUI Function Type
    GUI, onSubModel, getModel, getSubModel, putModel, modifyModel,
    changeRootHapplet, cancelNow, cancelIfBusy, howBusy, deleteEventHandler, bracketGUI,

    -- ** The Display Typeclass
    Display(..), DrawableRef(..), drawableEmptyRef,

    -- ** Workers and Threads
    -- $Workers
    CanRecruitWorkers(..),

    -- *** Thread "Workspaces"
    Workspace, newWorkspace, copyWorkspace,
    WorkerName, guiWorker, recruitWorker, relieveWorker, relieveWorkersIn,
    WorkCycleTime(..),

    -- *** Posts: where assigned workers can do work
    WorkPost, postWorkersUnion, pushWork, checkWork,
    postWorkers, postTaskWorkers, guiPostWorkers, closeWorkPost,
    WorkPostStats(..), getWorkPostStats, resetWorkPostStats, instantThroughput, totalThroughput,
    WorkerTask,  govWorker, taskPause, thisWorker, taskDone, taskSkip, taskFail, taskCatch,

    -- *** The 'Worker' abstraction
    Worker, workerThreadId, workerName, relieveGovWorkers,
    WorkerStatus(..), getWorkerStatus, workerNotBusy, workerNotDone, workerBusy, workerFailed,

    -- *** Unions: Groups of Worker Threads
    WorkerUnion, newWorkerUnion, unionizeWorker, retireWorker,

    -- *** Worker Providers
    WorkerSignal(..), WorkerNotification(..), setWorkerStatus, runWorkerTask,
    WorkerID, workerIDtoInt,

    -- * Functions for Providers
    MonadProvider(..), EventSetup(..), simpleSetup, installEventHandler, switchContext,

    -- ** Low-level details
    -- $LowLevel_Details
    GUIState(..), EventHandlerControl(..),
    guiModel, guiProvider, guiIsLive, execGUI, runGUI, guiCatch,
    makeHapplet, onHapplet, peekModel, sameHapplet,
    getGUIState, putGUIState, modifyGUIState, askHapplet, govWorkerUnion,

    -- *** Locking and Updating a Provider's State
    --
    -- This functions are called by 'installEventHandler', you should not need to use them yourself.
    ProviderStateLock, runProviderOnLock, providerLiftGUI, liftGUIProvider, checkGUIContinue,
  ) where

import           Prelude hiding ((.), id)

import           Happlets.Provider.React
import           Happlets.View.SampCoord

import           Control.Arrow
import           Control.Category
import           Control.Concurrent (ThreadId, myThreadId, yield, forkOS, threadDelay, throwTo)
import           Control.Concurrent.MVar
import           Control.Concurrent.QSem
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import qualified Control.Monad.Fail    as Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List             (intercalate)
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Data.Text             as Strict
import           Data.Time.Clock
import           Data.Typeable

import           System.IO.Unsafe (unsafePerformIO) -- used for generating unique Happlet IDs.

----------------------------------------------------------------------------------------------------

-- | This is a handle containing the current @model@ state of the 'Happlet'. It is constructed with
-- 'makeHapplet', but it is better to use 'Happlets.Initialize.makeHapplet' in the
-- "Happlets.Initialize" module in order to construct a 'Happlet' and associate it with a @provider@
-- in a single step.
data Happlet model
  = SubHapplet
    { happletId   :: !Int
    , happletLock :: MVar ThreadTracker
    }
    -- ^ Used as a place-holder in the 'GUIState' when no MVar locking is necessary to access parts
    -- of the GUI model value.
  | Happlet
    { happletId   :: !Int
    , happletLock :: MVar ThreadTracker
    , happletMVar :: MVar model
    }
  deriving (Eq, Typeable)

data ThreadTracker = ThreadTracker{ threadCount :: !Int, threadList :: ![ThreadId] }

-- | Create a 'SubHapplet' from a 'Happlet'.
subHapplet :: Happlet model -> Happlet subModel
subHapplet = \ case
  SubHapplet{ happletId=hid, happletLock=lock } -> SubHapplet{ happletId = hid, happletLock = lock }
  Happlet{ happletId=hid, happletLock=lock }    -> SubHapplet{ happletId = hid, happletLock = lock }

newThreadTracker :: ThreadTracker
newThreadTracker = ThreadTracker{ threadCount = (-1), threadList = [] }

addThread :: MVar ThreadTracker -> ThreadId -> IO ()
addThread lock this = modifyMVar_ lock $ \ st -> return $ st
  { threadCount = threadCount st + 1
  , threadList  = this : threadList st
  }

removeThread :: MVar ThreadTracker -> ThreadId -> IO ()
removeThread lock this = modifyMVar_ lock $ \ st -> return $ st
  { threadCount = max (-1) (threadCount st - 1)
  , threadList  = filter (/= this) (threadList st)
  }

happletIdGen :: MVar Int
happletIdGen = unsafePerformIO $ newMVar minBound
{-# NOINLINE happletIdGen #-}

-- | It is possible to switch which 'Happlet' is attached to a window, and the window need not
-- attach to a window with a 'Happlet' of the same type as was attached before, the type may
-- switch. However just because the type could switch doesn't necessarily mean a careless 'Happlet'
-- programmer will ask the window to from the current happlet to itself. Since switching occurs
-- while the 'Happlet' is locked, doing this could result in a deadlock. So a method of being able
-- to tell whether two happlets are exactly the same at runtime has been devised, so that a simple
-- test can be performed to see if the window is being asked to re-attach from a 'Happlet' to the
-- same 'Happlet'.
sameHapplet :: Happlet a -> Happlet b -> Bool
sameHapplet a b = happletId a == happletId b

-- | Construct a new 'Happlet'. The given initial 'Happlets.GUI.GUI' function is stored with the
-- 'Happlet' value itself, it is not stored in a reference.
makeHapplet :: model -> IO (Happlet model)
makeHapplet mod = do
  mvar   <- newMVar mod
  lock   <- newMVar newThreadTracker
  happId <- modifyMVar happletIdGen $ return . (id &&& id) . (+ 1)
  return Happlet{ happletId = happId, happletLock = lock, happletMVar = mvar }

-- | Lock the 'Happlet' and perform an IO function on the content of it.
onHapplet :: Happlet model -> (model -> IO (a, model)) -> IO (a, model)
onHapplet hap f = do
  let mvar = happletMVar hap
  let lock = happletLock hap
  this <- myThreadId
  bracket_ (addThread lock this) (removeThread lock this) $ do
    modifyMVar mvar $ liftM (\ (a, model) -> (model, (a, model))) . f

-- | Create a copy of the @model@ contained within the 'Happlet' from the current thread. Since this
-- @model@ is always being updated by various other threads, the value returned will only be a
-- snapshot of the @model@ at a particular point in time.
--
-- For the most part, a @model@ is only useful to the various 'Happlets.GUI.GUI' threads that run in
-- response to input events, but if it is necessary to view a snapshot of the @model@ from outside
-- of the event 'Happlet.GUI.GUI' proper, this is the function to use.
--
-- Note that if you evaluate this function using 'liftIO' on a 'Happlet' within a 'GUI' function
-- that is currently operating on this 'Happlet', this function will throw an exception.
peekModel :: Happlet model -> IO model
peekModel = \ case
  Happlet{happletMVar=mvar} -> readMVar mvar
  SubHapplet{happletId=hid} -> error $
    "'Happlets.GUI.peekModel' evaluated on locked 'Happlet' (happletId="++show hid++")"

-- | Every thread that is waiting to access the @model@ of a 'Happlet' is recorded. This function
-- returns the number of threads that are waiting for access.
getWaitingThreads :: Happlet model -> IO (Int, [ThreadId])
getWaitingThreads hap = (threadCount &&& threadList) <$> readMVar (happletLock hap)

modifyMVar' :: MVar st -> (st -> IO (a, st)) -> IO a
modifyMVar' mvar f = modifyMVar mvar $ fmap (\ (a, st) -> (st, a)) . f

----------------------------------------------------------------------------------------------------

-- | This is a type class similar to the 'Prelude.Show' class, except it displays to the 'GUI'.
class Display drawable where
  -- | When something is displayed to the GUI, an object should be inserted into the @model@ of the
  -- GUI, and then the canvas should be redrawn using the functions in
  -- 'Happlet.Control.Window.HappletWindow'. This function should return a reference to the drawable
  -- so that it can be updated and/or deleted.
  display :: drawable -> GUI provider model (DrawableRef provider model)

data DrawableRef provider model
  = DeleteDrawable
    { deleteDrawable :: GUI provider model ()
    , updateDrawable :: GUI provider model ()
    }

-- | A 'DrawableRef' that does nothing, use Haskell record syntax to provide the necessary functions.
drawableEmptyRef :: DrawableRef provider model
drawableEmptyRef = DeleteDrawable
  { deleteDrawable = return ()
  , updateDrawable = return ()
  }

----------------------------------------------------------------------------------------------------

-- | This is the function type used to make stateful updates to your Happlet. There are 'GUI'
-- functions that can control every single part of your GUI program state, including (but not
-- limited to)
--
-- * positioning and resizing the window
-- * installing and removing event handlers
-- * event handlers are also 'GUI' functions, so 'GUI' is used to react to events as well
-- * drawing to the window
-- * altering the @model@ contained within the happlet.
--
-- GUI is a monad function type that instantiates the 'Control.Monad.State.Class.MonadState'
-- allowing you to transform the value stored in the 'Happlet'. Simply use
-- 'Control.Monad.State.Class.get' and 'Control.Monad.State.Class.put' to update your program, and
-- then call functions like 'onCanvas' to re-draw relavent parts of the @provider@.
--
-- The function 'deleteEventHandler' can be used to immediately halt evaluation of a 'GUI' function,
-- and delete the current event handler, which can be useful when you write your event
-- handlers. There is also the similar 'cancelNow' function which immediately halts evaluation but
-- does not delete the event handler.
--
-- 'GUI' instantiates 'Control.Monad.Except.MonadExcept' so you can throw and catch error strings
-- (of type 'Strict.Text'). You will notice that 'GUI' does not instantiate
-- 'Control.Monad.Except.ExceptT', 'Control.Applicative.Alternative', or 'Control.Monad.MonadPlus',
-- because the 'GUI' cannot promise to obey the laws of these type classes. For example: if you were
-- to write the code:
--
-- @
-- -- WARNING: this code will fail to compile.
-- ('onCanvas' drawRedCircle >> cancelNow) 'Control.Applicative.<|>'
--   ('onCanvas' drawBlueCircle >> return ())
-- @
--
-- The above code is not allowed because once the @drawRedCircle@ is evaluated it cannot be undone
-- even though 'cancelNow' was called. Evaluation then proceeds to @drawBlueCircle@, and the result
-- is bothe the red and blue circles are visible on screen, even through the red circle evaluation
-- was canceled.
--
-- Usually you will use a type synonym specific to the Happlets provider that you are using. For
-- example, the Gtk+ provider for Happlets (not included in this package, must be downloaded and
-- installed separately) creates a type synonym:
--
-- @
-- type GtkGUI model a = GUI GtkWindow model a
-- @
--
-- However, if you want to createa a "universal" Happlet that works with absolutely any provider,
-- not just Gtk+, then your entire program should be written with a polymorphic @provider@ type.  The
-- @provider@ type is the type specific to the operating system and the Happlet provider you use, so
-- if you leave it blank, you will be free to call 'simpleHapplet' with any
-- 'Happelt.Provider.Provider' at all. However, at this time, the Happlets API is a little too
-- sparse for this to be practical, for the time being, your program may need to rely on
-- functionality specific to a Provider like Gtk+.
newtype GUI provider model a
  = GUI{ unwrapGUI :: StateT (GUIState provider model) IO (EventHandlerControl a)}
  deriving (Functor)

-- | A data type indicating the condition of GUI evaluation. This value is part of the 'GUIState'
-- and is usually set when calling 'breakGUI'.
data EventHandlerControl a
  = EventHandlerContinue a
    -- ^ Allow the event handler to remain enabled for the next incoming event.
  | EventHandlerHalt
    -- ^ Disable the event handler
  | EventHandlerCancel
    -- ^ Like 'EventHandlerHalt' but does not disable the event handlers.
  | EventHandlerFail !Strict.Text
    -- ^ report a failure
  deriving (Eq, Show, Functor)

-- | The 'GUIState' is the state that is constructed during the evaluation of a GUI function.
data GUIState provider model
  = GUIState
    { theGUIModel    :: !model
    , theGUIHapplet  :: !(Happlet model)
    , theGUIProvider :: !(ProviderStateLock provider)
    , theGUIWorkers  :: !WorkerUnion
    }

instance Applicative (GUI provider model) where { pure = return; (<*>) = ap; }

instance Monad (GUI provider model) where
  return = GUI . return . EventHandlerContinue
  (GUI f) >>= next = GUI $ StateT $ \ st -> runStateT f st >>= \ (a, st) -> case a of
    EventHandlerContinue a -> runStateT (unwrapGUI $ next a) st
    EventHandlerHalt       -> return (EventHandlerHalt    , st)
    EventHandlerCancel     -> return (EventHandlerCancel  , st)
    EventHandlerFail   err -> return (EventHandlerFail err, st)

instance MonadIO (GUI provider model) where
  liftIO f = GUI $ liftIO $ EventHandlerContinue <$> f

instance MonadState model (GUI provider model) where
  state f = GUI $ state $ \ st ->
    let (a, model) = f (theGUIModel st) in (EventHandlerContinue a, st{ theGUIModel = model })

instance MonadReader (Happlet model) (GUI provider model) where
  ask = GUI $ EventHandlerContinue <$> gets theGUIHapplet
  local loc (GUI f) = GUI $ do
    oldst <- get
    put $ oldst{ theGUIHapplet = loc $ theGUIHapplet oldst }
    f <* modify (\ newst -> newst{ theGUIHapplet = theGUIHapplet oldst })
  
instance Monad.MonadFail (GUI provider model) where
  fail = GUI . return . EventHandlerFail . Strict.pack

instance MonadError Strict.Text (GUI provider model) where
  throwError = GUI . return . EventHandlerFail
  catchError (GUI try) catch = GUI $ try >>= \ case
    EventHandlerFail msg -> unwrapGUI $ catch msg
    result               -> return result

instance Semigroup a => Semigroup (GUI provider model a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (GUI provider model a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

-- | 'Control.Lens.Lens' for manipulating the 'GUIState'. It is better to use 'getModel',
-- 'updateModel', 'putModel', 'subModel', or 'liftGUI' instead of manipulating a 'GUIState'
-- directly.
guiModel :: Lens' (GUIState provider model) model
guiModel = lens theGUIModel $ \ a b -> a{ theGUIModel = b }

-- | 'Control.Lens.Lens' for manipulating the 'GUIState'. It is better to not use this function at
-- all.
guiProvider :: Lens' (GUIState provider model) (ProviderStateLock provider)
guiProvider = lens theGUIProvider $ \ a b -> a{ theGUIProvider = b }

-- | Evaluate a 'GUI' function on a given 'Happlet'. The 'Happlet' is __NOT__ locked during
-- evaluation, it is expected that the 'Happlet' has already been locked with 'onHapplet', however a
-- copy of the 'Happlet' must be passed along with it's content to this function in order to
-- evaluate it. This is analogous to 'Control.Monad.State.runStateT' in the "Control.Monad.State"
-- module.
runGUI
  :: GUI provider model a -- ^ the 'GUI' function to evaluate
  -> GUIState provider model
  -> IO (EventHandlerControl a, GUIState provider model)
runGUI (GUI f) st = runStateT f st

-- | Like 'runGUI' but discards the @a@ typed return value. This is analogous to
-- 'Control.Monad.State.execStateT' in the "Control.Monad.State" module.
execGUI
  :: GUI provider model a -- ^ the 'GUI' function to evaluate
  -> GUIState provider model
  -> IO (GUIState provider model)
execGUI f = fmap snd . runGUI f

-- | Evaluate a 'GUI' function but catch calls to 'cancelNow', 'deleteEventHandler', and
-- 'Control.Monad.Fail.fail'.
guiCatch
  :: GUI provider model a
  -> (EventHandlerControl a -> GUI provider model b)
  -> GUI provider model b
guiCatch (GUI f) = ((GUI $ f >>= \ result -> return (EventHandlerContinue result)) >>=)

-- | Like the 'Control.Exception.bracket' function, but works in the 'GUI' monad.
bracketGUI
  :: GUI provider model open
  -> GUI provider model closed
  -> GUI provider model a
  -> GUI provider model a
bracketGUI lock unlock f = GUI $ StateT $ \ st -> do
  mvar <- newEmptyMVar
  a    <- bracket
    (runGUI lock st >>= \ (rsrc, st) -> putMVar mvar st >> return rsrc)
    (const $ modifyMVar' mvar $ runGUI unlock)
    (\ case
      EventHandlerContinue{} -> modifyMVar' mvar $ runGUI f
      EventHandlerHalt       -> return EventHandlerHalt
      EventHandlerCancel     -> return EventHandlerCancel
      EventHandlerFail err   -> return $ EventHandlerFail err
    )
  st <- takeMVar mvar
  return (a, st)

-- | Operate on a portion of the @model@, which can be updated by a given 'Lens' into a @subModel@
-- of the @model@, by evaluating a 'GUI' function on that @subModel@.
onSubModel 
  :: Lens' model subModel
  -> GUI provider subModel a
  -> GUI provider model a 
onSubModel submodel subgui = do
  guist <- getGUIState
  let modst = theGUIModel guist ^. submodel
  (a, new) <- liftIO $ runGUI subgui $ GUIState
    { theGUIModel    = modst
    , theGUIHapplet  = subHapplet $ theGUIHapplet guist
    , theGUIProvider = theGUIProvider guist
    , theGUIWorkers  = theGUIWorkers  guist
    }
  putGUIState $ guist
    { theGUIModel    = theGUIModel    guist & submodel .~ theGUIModel new
    , theGUIHapplet  = theGUIHapplet  guist
    , theGUIProvider = theGUIProvider guist
    , theGUIWorkers  = theGUIWorkers  guist
    }
  GUI $ return a

-- | Once you install an event handler, the default behavior is to leave the event handler installed
-- after it has evaluated so that it can continue reacting to events without you needing to
-- re-install the event handler after each reaction.
--
-- However if you wish for an event handler to remove itself, evaluate this function as the final
-- function call in your 'GUI' procedure. Calling this function tells your 'GUI' function to
-- immediately halt and remove itself from the event handling loop.
--
-- Under the hood, this function will evaluate 'breakGUI' with a 'EventHandlerHalt' condition set in the
-- 'guiContinue' field.
deleteEventHandler :: GUI provider model void
deleteEventHandler = GUI $ return EventHandlerHalt

-- | Cancel the current event handler execution. This function is like 'disable' but does not
-- instruct the event handler to remove itself.
cancelNow :: GUI provider model void
cancelNow = GUI $ return EventHandlerCancel

-- | When an event handler evaluates this function, it is voluntarily canceling the current event
-- handler execution if there are one or more other threads waiting for access to the 'Happlet'
-- @model@. This function should be evaluated after making important updates to the @model@
-- (especially ones that indicate which events have been handled), but before time-consuming drawing
-- updates have begun. This allows other threads to have access to the model and perform their own
-- drawing.
cancelIfBusy :: GUI provider model ()
cancelIfBusy = howBusy >>= flip when cancelNow . (> 0)

-- | Return a value indicating how many pending threads there are waiting for their turn to access
-- the 'Happlet' @model@.
howBusy :: GUI provider model Int
howBusy = GUI $ fmap EventHandlerContinue $
  gets theGUIHapplet >>= liftIO . fmap fst . getWaitingThreads

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.get'. However this function
-- is provided as synonym for the 'Control.Monad.State.Class.get' function for convenience, and
-- perhaps for better readability in your code.
getModel :: GUI provider model model
getModel = get

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.gets'. However this function
-- is provided as synonym for the 'Control.Monad.State.Class.gets' function for convenience, and
-- perhaps for better readability in your code.
getSubModel :: (model -> a) -> GUI provider model a
getSubModel = gets

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.put'. However this function
-- is provided as synonym for the 'Control.Monad.State.Class.put' function for convenience, and
-- perhaps for better readability in your code.
putModel :: model -> GUI provider model ()
putModel = put

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.modify'. However this
-- function is provided as synonym for the 'Control.Monad.State.Class.modify' function for
-- convenience, and perhaps for better readability in your code.
modifyModel :: (model -> model) -> GUI provider model ()
modifyModel = modify

-- | Force the OS window (view port) to change the 'Happlet' model that is currently being displayed
-- as the root model. This function takes the new 'Happlet' and an initializer action which must
-- setup the event handlers for the OS window to use with the new 'Happlet' model.
--
-- Note that this function evaluates to a 'GUI' function type for the @oldModel@, so expected that
-- this function will never return, rather the next 'GUI' functions to be evaluated in this OS
-- window will be the event handlers for the @newModel@ set by the initializer given to this
-- function.
changeRootHapplet
  :: forall m provider newModel oldModel
  .  (MonadProvider provider m, CanRecruitWorkers provider)
  => Happlet newModel -> (PixSize -> GUI provider newModel ()) -> GUI provider oldModel ()
changeRootHapplet newHapp init = do
  -- Here we set the 'contextSwithcer' field to a function that evaluates 'switchContext'.
  modifyGUIState $ \ guist -> case guist ^. guiProvider of
    ProviderUnlocked{} -> error "changeRootHapplet was evaluated on an unlocked provider state"
    ProviderLocked env -> guist & guiProvider .~
      ( ProviderLocked
        (env & contextSwitcher .~ switchContext (env ^. detatchHandler) newHapp init)
      )
  cancelNow
  -- Here ^ we use 'cancelNow' instead of 'deleteEventHandler' because 'disconnectAll' was already
  -- called so we do not want any disconnect function to be evaluated again (which shouldn't cause
  -- any harm, but doing so is an unnnecessary step). It is also for this reason that the
  -- 'providerLiftGUI' function only bothers to evaluate the context switcher when the return
  -- signal is 'HappletEventCancel'. Please refer to the 'liftGUIintoGtkState' function to see
  -- exactly what happens next, after the above 'cancelNow' is evaluated.

----------------------------------------------------------------------------------------------------

-- $LowLevel_Details
--
-- These are functions that should only be used by the back-end
-- 'Happlets.Provider.Provider'. If you think you need to use these functions when programming an
-- ordinary Happlet, ask an expert how you might solve your problem without using them.

-- | Tests if a the 'GUIState' produced after evaluating 'GUI' function with 'evalGUI' is still
-- ready to receive further input signals.
guiIsLive :: EventHandlerControl a -> Bool
guiIsLive = \ case { EventHandlerContinue{} -> True; _ -> False; }

-- | Obtain a copy of the entire 'GUIState' data structure set for the current 'GUI' evaluation
-- context. This function should never be used unless you are programming a Happlet
-- 'Happlets.Provider.Provider'.
getGUIState :: GUI provider model (GUIState provider model)
getGUIState = GUI $ EventHandlerContinue <$> get

-- | Update the entire copy of the 'GUIState' data structure for the current context. This
-- function should never be used unless you are programming a Happlet 'Happlets.Provider.Provider'.
putGUIState :: GUIState provider model -> GUI provider model ()
putGUIState = GUI . fmap EventHandlerContinue . put

-- | Modify the 'GUIState' data structure for the current context. This function should never be
-- used unless you are programming a Happlet 'Happlets.Provider.Provider'.
modifyGUIState :: (GUIState provider model -> GUIState provider model) -> GUI provider model ()
modifyGUIState = GUI . fmap EventHandlerContinue . modify

-- | Obtain a reference to the 'Happlet' in which this 'GUI' function is being evaluated.
askHapplet :: GUI provider model (Happlet model)
askHapplet = ask

-- | Obtain a reference to the "government" 'WorkerUnion' for this GUI.
govWorkerUnion :: GUI provider model WorkerUnion
govWorkerUnion = GUI $ EventHandlerContinue <$> gets theGUIWorkers

----------------------------------------------------------------------------------------------------

class (MonadIO m, MonadState provider m) => MonadProvider provider m | provider -> m where
  -- | This function should work like 'runStateT' for the function type @m@.
  runProviderIO :: m a -> provider -> IO (a, provider)

  -- | When a context switch is performed, this value is set with a continuation to be called after
  -- the 'windowChangeHapplet' function completes. After evaluation of the 'GUI' function completes,
  -- the state is evaluated -- every single event handler will evaluate this function. To ensure
  -- that nothing happens unless it is set, this lens is used to set the callback to @return ()@ (a
  -- no-op) prior to evaluation of the 'GUI' procedure in the 'providerLiftGUI' function.
  contextSwitcher :: Lens' provider (m (EventHandlerControl ()))

  -- | The 'provider' data structure must contain a reference to an 'MVar' which contains
  -- itself. This 'MVar' is to be shared by all threads that have access to the @provider@'s state,
  -- especially threads being evaluated by callback functions that are executed in response to
  -- events by the low-level system executive.
  providerSharedLock :: provider -> MVar provider

  -- | Should report that a callback function failed, and report the reason for failure.
  signalFatalError :: Strict.Text -> m ()

  -- | Return the size of the viewport window provided to the Happlet by the low-level system
  -- executive.
  getProviderWindowSize :: m PixSize

  -- | All providers will probably have a reaction for initializing the Happlet, which should get
  -- called after the window system has been initialized, and resources for the Happlet window have
  -- been allocated and are ready to begin drawing. The event type of the 'ConnectReact' action is
  -- the size of the window in which the 'Happlet' will be drawn.
  initReaction :: MonadState provider m => Lens' provider (ConnectReact m PixSize)

  -- | All providers will probably have a reaction for cleaning-up when the Happlet is shut down, or
  -- when a context switch occurs. The event type of the 'ConnectReact' action is @()@.
  detatchHandler :: MonadState provider m => Lens' provider (ConnectReact m ())

  -- | All providers will probably have more than one 'ConnectReact' value stored within it's
  -- state. This function must call 'forceDisconnect' on every 'ConnectReact' value wihin the
  -- @provider@'s state.
  disconnectAll :: MonadState provider m => m ()

data ProviderStateLock provider
  = ProviderUnlocked (MVar provider)
    -- ^ Unocked means we have not yet called 'modifyMVar', but we can do so on the 'MVar' given
    -- here.
  | ProviderLocked   provider
    -- ^ Locked means we are within 'modifyMVar' and we have the @provider@ state value.

-- | Lock a 'ProviderStateLock' and then evaluate 'runProviderIO' on the given 'MonadProvider'
-- function @m@.
runProviderOnLock
  :: (MonadIO m, MonadProvider provider m)
  => ProviderStateLock provider -> m a -> IO a
runProviderOnLock lock f = case lock of
  ProviderUnlocked mvar -> modifyMVar mvar $ fmap (\ (a,b) -> (b,a)) . runProviderIO f
  ProviderLocked{}      -> fail $ "runProviderOnLock: evaluated on already locked provider."

-- | This function should always be called within a callback function that is installed into the
-- provider for handling events coming into the program from the operating system.
liftGUIProvider
  :: (MonadIO m, MonadState provider m, MonadProvider provider m)
  => m a -> GUI provider model a
liftGUIProvider f = getGUIState >>= \ gui -> case theGUIProvider gui of
  ProviderUnlocked{} -> error $ "liftGUIProvider: " ++
    "Evaluated a provider function within a GUI function on a locked ProviderStateLock."
  ProviderLocked env -> do
    (a, env) <- liftIO $ runProviderIO f env
    putGUIState $ gui{ theGUIProvider = ProviderLocked env }
    return a

-- | This function evaluates 'GUI' functions within event handlers. It evaluates to a function of
-- type @m@ (which must be a stateful function that has a @provider@ as part of it's state), which
-- means you are required to have first evaluated 'runProviderOnLock'. This function will then lock
-- the 'Happlet' and then evaluate the 'GUI' function. The 'EventHandlerControl' returned indicates
-- how the evaluation of the 'GUI' function was terminated.
providerLiftGUI
  :: (MonadIO m, MonadState provider m, MonadProvider provider m, CanRecruitWorkers provider)
  => Happlet model -> GUI provider model a -> m (EventHandlerControl a)
providerLiftGUI happlet f = do
  -- (1) Always set 'contextSwitcher' to a no-op, it may be set to something else by @f@.
  contextSwitcher .= return (EventHandlerContinue ())
  -- (2) Get the provider after having set it's 'contextSwitcher' to a no-op.
  provider <- get
  (result, provider) <- liftIO $ fmap fst $ onHapplet happlet $ \ model -> do
    (guiContin, guiState) <- runGUI f GUIState
      { theGUIHapplet  = happlet
      , theGUIProvider = ProviderLocked provider
      , theGUIModel    = model
      , theGUIWorkers  = governmentWorkerUnion provider
      }
    case theGUIProvider guiState of
      ProviderLocked provider -> return ((guiContin, provider), theGUIModel guiState)
      ProviderUnlocked{}      -> fail $
        "providerLiftGUI: runGUI succesfull, but provider in GUIState is still locked"
  -- (3) Put the newly updated provider state back into the monad state
  put provider
  -- (4) Check if the result is 'HappletEventCancel', if it is, there is a chance that a context
  -- switch occurred. The context switcher is always evaluated on a cancel signal. If the context
  -- switcher was not changed by @f@ between (1) and (2), the 'contextSwitcher' field has therefore
  -- not been changed from the (return ()) value (a no-op) set at (1) and so no context switch
  -- occurs. (See the 'changeRootHapplet' function for the mechanism we are actuating here).
  case result of
    EventHandlerCancel -> (provider ^. contextSwitcher) >>= \ case
      EventHandlerFail msg -> signalFatalError msg
      _                    -> return ()
    _                  -> return ()
  return result

-- | This function is intended to be used within the 'reactTo' action of a 'ConnectReact' function.
-- If the 'GUI' function evaluation given to 'providerLiftGUI' evaluates to 'fail' or 'empty', the
-- callback for this particular event type needs to be removed and unregistered from the low-level
-- system executive, and the 'ConnectReact' needs to be set to 'Disconected' within the @provider@
-- state, which is what this function does.
--
-- Call this function within the 'doReact' function, which is installed into a callback that is
-- executed by the low-level system executive when an event is received, it will call
-- 'providerLiftGUI' to begin evaluating the 'GUI' reaction function for that event type. The result
-- of 'providerLiftGUI' function should be passed to this 'checkGUIContinue' function, if it is
-- 'EventHandlerHalt' or 'EventHandlerFail', then 'forceDisconnect' is called on the 'ConnectReact'
-- function for this lens.
checkGUIContinue
  :: (MonadIO m, MonadProvider provider m)
  => Lens' provider (ConnectReact m event) -> EventHandlerControl a -> m ()
checkGUIContinue connectReact = \ case
  EventHandlerHalt       -> forceDisconnect connectReact
  EventHandlerFail   msg -> forceDisconnect connectReact >> signalFatalError msg
  EventHandlerCancel     -> return ()
  EventHandlerContinue{} -> return ()

-- | This function constructs a stateful @provider@ function of type @m@ which performs a context
-- switch. The constructed function @m@ function is stored in the 'contextSwitcher' field of the
-- @provider@. Before 'contextSwitcher' is replaced, the old 'contextSwitcher' is evaluated. The
-- result of this function is the result of the evaluation of the old 'contextSwitcher'.
switchContext
  :: (MonadIO m, MonadProvider provider m, CanRecruitWorkers provider)
  => ConnectReact m ()
    -- ^ The ation to be evaluated when this next 'Happlet' is detached. The @event@ type of this
    -- action is @()@.
  -> Happlet newmodel
    -- ^ The next 'Happlet' to take control over the GUI.
  -> (PixSize -> GUI provider newmodel ())
    -- ^ Initailizer that sets up event handlers for the next 'Happlet'
  -> m (EventHandlerControl ())
switchContext detatch newHapp init = do
  case detatch of
    ConnectReact{reactTo=detatch} -> detatch ()
    Disconnected                  -> pure ()
  disconnectAll
  env  <- get
  size <- getProviderWindowSize
  (>>= state . const) $ liftIO $ fmap fst $ onHapplet newHapp $ \ model -> do
    (guiContin, guist) <- runGUI (init size) GUIState
      { theGUIProvider = ProviderLocked env
      , theGUIHapplet  = newHapp
      , theGUIModel    = model
      , theGUIWorkers  = governmentWorkerUnion env
      }
    case theGUIProvider guist of
      ProviderLocked winst -> return ((guiContin, winst), theGUIModel guist)
      ProviderUnlocked{}   -> fail $
        "'runGUI' was given an unlocked provider state, but it returned a locked provider state"

----------------------------------------------------------------------------------------------------

data EventSetup provider m event model
  = EventSetup
    { eventDescription  :: String
      -- ^ A human-readble description of which event handler installer this is, to be reported in
      -- debug logs.
    , eventLensReaction :: Lens' provider (ConnectReact m event)
      -- ^ A lens that can get and set one of the 'ConnectReact' functions in the Gtk window state.
    , eventPreInstall   :: m ()
      -- ^ An action to perform prior installing the Gtk event handler callback.
    , eventInstall      :: provider -> (event -> IO ()) -> IO (IO ())
      -- ^ Register an event handler callback which triggers event reactions into the low-level
      -- system executive. This function must return another function which un-registers this
      -- callback.
    , eventGUIReaction  :: event -> GUI provider model ()
      -- ^ The GUI function provided by the end users (programmers) of this library that reacts to
      -- the event with a state update.
    }

-- | An 'EventSetup' that does nothing more than install a 'ConnectReact' function.
simpleSetup
  :: (MonadIO m, MonadProvider provider m)
  => Lens' provider (ConnectReact m event)
  -> (event -> GUI provider model ())
  -> EventSetup provider m event model
simpleSetup connectReact react = EventSetup
  { eventDescription  = ""
  , eventLensReaction = connectReact
  , eventPreInstall   = return ()
  , eventInstall      = \ _ _ -> return $ pure ()
  , eventGUIReaction  = react
  }

-- | This function enables an event handler, and takes a continuation which will be evaluated by
-- this function call to do the low-level work of install the event handler into the Gtk
-- window. This function also does the work of taking a 'Happlets.GUI.GUI' function to be evaluated
-- on each event, converting this function to a 'ConnectReact' function, and installing it into the
-- 'GtkWindow's internal state so that it can be actually evaluated every time the low-level event
-- handler is evaluated. The result is a function that can be used to instantiate any of the
-- "Happlets.GUI" module's event handler classes.
installEventHandler
  :: (MonadIO m, MonadProvider provider m, CanRecruitWorkers provider)
  => EventSetup provider m event model -> GUI provider model ()
installEventHandler setup = do
  happ <- askHapplet
  liftGUIProvider $ do
    forceDisconnect (eventLensReaction setup)
    eventPreInstall setup
    env <- get
    disconnect <- liftIO $ eventInstall setup env $
      -- Here we install an IO function into a callback. As soon as this callback is executed, the
      -- provider must be locked using 'runProviderOnLock' in order to begin evaluating the 'GUI'
      -- function associated with this event callback.
      runProviderOnLock (ProviderUnlocked $ providerSharedLock env) .
      evalConnectReact (eventLensReaction setup)
    eventLensReaction setup .= ConnectReact
      { doDisconnect = liftIO disconnect
      , reactTo =
          providerLiftGUI happ . eventGUIReaction setup >=>
          checkGUIContinue (eventLensReaction setup)
      }

----------------------------------------------------------------------------------------------------

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
  = WorkerTask { unwrapWorkerTask :: ReaderT Worker (StateT work IO) (EventHandlerControl a) }

-- | Evaluae a 'WorkerTask' using a given 'Worker' by locking an 'Control.Concurrent.MVar.MVar' and
-- evaluating the 'WorkerTask' function to work on the content of this
-- 'Control.Concurrent.MVar.MVar'.
runWorkerTask :: Worker -> WorkerTask work a -> MVar work -> IO (EventHandlerControl a)
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
  return = WorkerTask . return . EventHandlerContinue
  (WorkerTask a) >>= f = WorkerTask $ a >>= \ case
    EventHandlerContinue a -> unwrapWorkerTask $ f a
    EventHandlerHalt       -> return EventHandlerHalt
    EventHandlerCancel     -> return EventHandlerCancel
    EventHandlerFail   msg -> return $ EventHandlerFail msg
  fail   = taskFail . Strict.pack

instance MonadState work (WorkerTask work) where
  state = WorkerTask . fmap EventHandlerContinue . lift . state

-- | When a 'WorkerTask' is being evaluated, a 'Worker' may refer to one's self by obtaining a copy
-- of it's identification (a union membership card).
thisWorker :: WorkerTask work Worker
thisWorker = WorkerTask $ EventHandlerContinue <$> ask

-- | This function obtains status information from a given 'Worker'. The worker may update it's
-- own status at any time, so the returned 'WorkerStatus' is only a snapshot of the status at a
-- given time, it may even change the very moment after this function returns.
getWorkerStatus :: MonadIO m => Worker -> m WorkerStatus
getWorkerStatus = liftIO . readMVar . workerStatus

-- | The task should stop looping and close-up shop.
taskDone :: WorkerTask work void
taskDone = WorkerTask $ return EventHandlerHalt

-- | The task should skip this loop and wait for the next loop.
taskSkip :: WorkerTask work void
taskSkip = WorkerTask $ return EventHandlerCancel

-- | Signal a failure occured.
taskFail :: Strict.Text -> WorkerTask work void
taskFail = WorkerTask . return . EventHandlerFail

-- | Catch a 'taskSkip' or 'taskFail' signal, or really any signal at all.
taskCatch :: WorkerTask work a -> (EventHandlerControl a -> WorkerTask work b) -> WorkerTask work b
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
  return $ EventHandlerContinue ()

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

-- | Similar to 'recruitWorker' but works for the "government" and operates on the current @model@
-- in of the 'GUI' function which recruits this 'Worker'. This function is also similar to
-- 'govWorker', but notice that the task to be performed is not an ordinary 'WorkerTask' function,
-- but a 'GUI' function. This allows the worker to update the GUI directly, as if there were an
-- event handler constantly responding to a stream of events being generated in a parallel process.
--
-- The 'Happlet.Provider.Provider' must instantiate 'CanRecruitWorkers' in order for this function
-- to be usable -- we hope that all 'Happlet.Provider.Provider's will do this.
guiWorker
  :: CanRecruitWorkers provider
  => WorkerName -> WorkCycleTime
  -> GUI provider model void -> GUI provider model Worker
guiWorker handl cycle f = do
  guist <- getGUIState
  newWorker (launchWorkerThread guist) (theGUIWorkers guist) handl cycle $ \ worker -> do
    setWorkerStatus worker WorkerWaiting
    inOtherThreadRunGUI guist (liftIO (setWorkerStatus worker WorkerBusy) >> f)

-- | Similar to 'recruitWorker' but works in the "government" 'WorkerUnion'.
--
-- Notice that this function must be evaluate to a 'GUI' function, unlike many of the other
-- 'Worker'-related functions here which can be evaluated to any function type of the 'MonadIO' type
-- class. The default "government" 'WorkerUnion' is only available during 'GUI' evaluation.
govWorker
  :: CanRecruitWorkers provider
  => Workspace work -> WorkerName -> WorkCycleTime
  -> WorkerTask work void -> GUI provider model Worker
govWorker workspace handl cycle f = do
  st  <- getGUIState
  gov <- govWorkerUnion
  _recruitWorker (launchWorkerThread st) gov workspace handl cycle f

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

-- | Similar to 'postWorkers', but the task evaluated is of type 'GUI', allowing full access to the
-- 'GUI's state as each work item is posted.
guiPostWorkers
  :: CanRecruitWorkers provider
  => [WorkerName] -> (inWork -> GUI provider model outWork)
  -> GUI provider model (WorkPost inWork outWork)
guiPostWorkers names f = getGUIState >>= \ st ->
  postal (launchWorkerThread st) names $ \ self work -> do
    setWorkerStatus self WorkerWaiting
    inOtherThreadRunGUI st (setWorkerStatus self WorkerBusy >> f work)

-- not for export
postal
  :: MonadIO m
  => ForkWorkerThread outWork
  -> [WorkerName]
  -> (Worker -> inWork -> IO (EventHandlerControl outWork))
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
          EventHandlerContinue result -> seq result $! do
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
  -> (Worker -> IO (EventHandlerControl a))
     -- ^ the task to be perofmred during each work cycle.
  -> IO (IO ())    -- ^ returns a function that can be used to halt the thread that is forked here.

-- not for export
newWorker
  :: MonadIO m
  => ForkWorkerThread void
  -> WorkerUnion -> WorkerName -> WorkCycleTime
  -> (Worker -> IO (EventHandlerControl void))
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
  -> (Worker -> IO (EventHandlerControl void))
  -> IO (IO ())
defaultLaunchWorkerThread wu cycle getWorker f = do
  let delay t = threadDelay $ round $ t * 1000 * 1000
  let workerLoop self = do
        let loop   = setWorkerStatus self WorkerPaused >>
              (case cycle of { WorkCycleWait t -> delay t; _ -> blink; }) >>
              workerLoop self
        result <- f self
        case result of
          EventHandlerContinue{} -> loop
          EventHandlerCancel     -> loop
          EventHandlerHalt       -> void $ setWorkerStatus self WorkerHalted
          EventHandlerFail   msg -> void $ setWorkerStatus self $ WorkerFailed msg
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

-- | Like 'relieveWorkersIn' but the signals are sent to the workers default "government"
-- 'WorkerUnion'.
relieveGovWorkers :: (Worker -> Bool) -> GUI provider model ()
relieveGovWorkers toBeRelieved = govWorkerUnion >>= liftIO . flip relieveWorkersIn toBeRelieved

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

----------------------------------------------------------------------------------------------------

-- | This class must be instantiated by the 'Happlet.Provider.Provider' to evaluate 'GUI' functions
-- in 'Worker' threads. The 'guiWorker' makes use of this interface.
--
-- Although the 'GUI' function type instantiates 'liftIO', it is important that you never evaluate
-- 'threadDelay' or wait on an 'MVar' or 'QSem' within a 'GUI' function, or you will almost
-- certainly deadlock the application, due to the way most GUI applications perform locking on the
-- 'MVar's which contain the resources shared with the operating system.
--
-- To perform any @IO@ computation which may cause the thread to block, or otherwise require a lot
-- of time to run to completion, call 'forkGUI' which creates a thread-safe evaluator for functions
-- of type 'GUI' that can be safely evaluated in the @IO@ context of the thread.
class CanRecruitWorkers provider where
  governmentWorkerUnion :: provider -> WorkerUnion

  -- | This function creates a new Haskell thread and passes an evaluator (a continuation function)
  -- of type @('GUI' window model a -> IO ('EventHandlerControl' a)@. This Haskell thread can safely
  -- spawn other threads, it can evaluate 'threadDelay', it can safely wait on 'MVar's or 'QSem's,
  -- and it can loop indefinitely. When it comes time to perform an action in the 'GUI', the thread
  -- function can call the given evaluator function to perform an update in the GUI.
  forkGUI
    :: ((GUI provider model a -> IO (EventHandlerControl a)) -> IO ())
       -- ^ Use this function to evaluate a 'GUI' function within the @IO@ context of the 'Worker'
       -- thread.
    -> GUI provider model ThreadId
  -- | This function must assume that the given 'GUI' function will be evaluated in a new thread
  -- with the given 'GUIState'. This function must perform all locking of mutex variables necessary
  -- to evaluate the 'GUI' function, and update all mutexes with the results of evaluation.
  inOtherThreadRunGUI :: GUIState provider model -> GUI provider model a -> IO (EventHandlerControl a)
  -- | Launch a worker thread that can safely interact with the GUI. Many GUI libraries do not
  -- provide thread safety, instead using a cooperative mutlithreading model, and so launching
  -- worker threads must make use of the GUI's own cooperative multithread functionality. In Gtk+
  -- trying to use Haskell threads all but guarantees deadlocks, or race conditions which lead to
  -- segmentation faults.
  --
  -- This function takes a delay time, an @IO ('EventHandlerControl' void)@ (usually a function
  -- evaluated by 'inOtherThreadRunGUI' above) which is called repeatedly and returns a control
  -- value that can optionally stop it's own thread loop, and returns an @IO ()@ function that can
  -- be used to halt the thread.
  launchWorkerThread
    :: GUIState provider model -- ^ provides access to the @provider@
    -> WorkerUnion
       -- ^ use to call 'unionizeWorker' when thread begins and 'retireWorker' when thread
       -- ends. Usually this union is the same as 'theGUIWorkers' ("government" union), but not
       -- necessarily, therefore it is passed as an argument to this function.
    -> WorkCycleTime -- ^ requests the cycle time of the work thread that is forked here.
    -> IO Worker
        -- ^ used to obtain a copy of the 'Worker' which represents thread forked here.
    -> (Worker -> IO (EventHandlerControl void))
        -- ^ the task to be perofmred during each work cycle.
    -> IO (IO ())    -- ^ returns a function that used to halt the thread that is forked here.
