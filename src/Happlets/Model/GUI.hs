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
    Happlet, makeHapplet, onHapplet, peekModel, switchContext,
    sameHapplet, getWaitingThreads,

    -- * The GUI Function Type
    GUI, onSubModel, getModel, getSubModel, putModel, modifyModel,
    changeRootHapplet, cancelNow, cancelIfBusy, howBusy, deleteEventHandler,

    -- ** GUI Threads
    SendGUISignal, forkGUI, bracketGUI, guiCatch,

    -- ** The Display Typeclass
    Display(..), DrawableRef(..), drawableEmptyRef,

    -- * Functions for Providers
    liftGUIProvider, providerLiftGUI,
    EventHandlerControl(..), MonadProvider(..),
    EventSetup(..), simpleSetup, installEventHandler,

    -- ** Multi-Threading
    ProviderSyncCallback(..), ProviderStateLock, initProviderState, runProviderOnLock,

    --ProviderStateLock, runProviderOnLock, checkGUIContinue,
    --GUIState(..), guiModel, guiProvider, guiIsLive, execGUI, runGUI,
    --getGUIState, putGUIState, modifyGUIState, askHapplet, govWorkerUnion,

    -- * Re-exports
    module Control.Monad.State,
    module Control.Monad.Except,
  ) where

import           Prelude hiding ((.), id)

import           Happlets.Provider.React
import           Happlets.View.SampCoord

import           Control.Arrow
import           Control.Category
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import qualified Control.Monad.Fail    as Monad
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Text             as Strict
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
    , theGUIProvider :: !provider
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

---- | 'Control.Lens.Lens' for manipulating the 'GUIState'. It is better to use 'getModel',
---- 'updateModel', 'putModel', 'subModel', or 'liftGUI' instead of manipulating a 'GUIState'
---- directly.
--guiModel :: Lens' (GUIState provider model) model
--guiModel = lens theGUIModel $ \ a b -> a{ theGUIModel = b }

-- | 'Control.Lens.Lens' for manipulating the 'GUIState'. It is better to not use this function at
-- all.
guiProvider :: Lens' (GUIState provider model) provider
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

---- | Like 'runGUI' but discards the @a@ typed return value. This is analogous to
---- 'Control.Monad.State.execStateT' in the "Control.Monad.State" module.
--execGUI
--  :: GUI provider model a -- ^ the 'GUI' function to evaluate
--  -> GUIState provider model
--  -> IO (GUIState provider model)
--execGUI f = fmap snd . runGUI f

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
--  , theGUIWorkers  = theGUIWorkers  guist
    }
  putGUIState $ guist
    { theGUIModel    = theGUIModel    guist & submodel .~ theGUIModel new
    , theGUIHapplet  = theGUIHapplet  guist
    , theGUIProvider = theGUIProvider guist
--  , theGUIWorkers  = theGUIWorkers  guist
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
  .  (MonadProvider provider m, ProviderSyncCallback provider m)
  => Happlet newModel -> (PixSize -> GUI provider newModel ()) -> GUI provider oldModel ()
changeRootHapplet newHapp init = do
  -- Here we set the 'contextSwithcer' field to a function that evaluates 'switchContext'.
  modifyGUIState $ \ guist -> guist &
    guiProvider . contextSwitcher .~
    switchContext (theGUIProvider guist ^. detatchHandler) newHapp init
  cancelNow
  -- Here ^ we use 'cancelNow' instead of 'deleteEventHandler' because 'disconnectAll' was already
  -- called so we do not want any disconnect function to be evaluated again (which shouldn't cause
  -- any harm, but doing so is an unnnecessary step). It is also for this reason that the
  -- 'providerLiftGUI' function only bothers to evaluate the context switcher when the return
  -- signal is 'HappletEventCancel'. Please refer to the 'liftGUIintoGtkState' function to see
  -- exactly what happens next, after the above 'cancelNow' is evaluated.

----------------------------------------------------------------------------------------------------

---- $LowLevel_Details
----
---- These are functions that should only be used by the back-end
---- 'Happlets.Provider.Provider'. If you think you need to use these functions when programming an
---- ordinary Happlet, ask an expert how you might solve your problem without using them.
--
---- | Tests if a the 'GUIState' produced after evaluating 'GUI' function with 'evalGUI' is still
---- ready to receive further input signals.
--guiIsLive :: EventHandlerControl a -> Bool
--guiIsLive = \ case { EventHandlerContinue{} -> True; _ -> False; }

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

---- | Obtain a reference to the "government" 'WorkerUnion' for this GUI.
--govWorkerUnion :: GUI provider model WorkerUnion
--govWorkerUnion = GUI $ EventHandlerContinue <$> gets theGUIWorkers

----------------------------------------------------------------------------------------------------

-- | Every Happlets provider must initialize the 'MonadProvider' class defining a monad that wraps
-- all stateful information necessary to interface Happlets to the low-level system executive. The
-- data structure that contains all of this state information must wrap it all in this
-- 'ProviderStateRef', and define exactly one 'ProviderStateRef' to be used for the duration of the
-- application process lifespan. Construct a 'ProviderStateRef' with the 'initProviderState'
-- function below.
--
-- Also, the 'MonadProvider' class has a function 'providerSharedState' which must produce the one
-- 'ProviderStateRef', and so it will be necessary for providers to store the 'ProviderStateRef'
-- within itself.
newtype ProviderStateLock provider = ProviderStateLock (MVar provider)
  deriving Eq

-- | This function creates a new 'ProviderStateLock', and then evaluates a continuation which must
-- initialize a new @provider@ data structure and return it. The continuation that initializes the
-- @provider@ state should also store the 'ProviderStateLock' within itself.
--
-- The 'ProviderStateLock' is not fully initialized until the given continution function returns, so
-- do NOT evaluate 'runProviderOnLock' within the initializer or it will result in a deadlock
-- condition. Clearly, the initailizer continuation which is constructing the @provider@ data
-- structure already has access to the unlocked @provider@ data structure, so there should never be
-- any need to call 'runProviderOnLock' from within the initializer anyway.
initProviderState :: (ProviderStateLock provider -> IO provider) -> IO (ProviderStateLock provider)
initProviderState init = do
  mvar <- newEmptyMVar
  let ref  = ProviderStateLock mvar
  init ref >>= putMVar mvar
  return ref

class (MonadIO m, MonadState provider m) => MonadProvider provider m | provider -> m where
  -- | This function should work like 'runStateT' for the function type @m@.
  runProviderIO :: m a -> provider -> IO (a, provider)

  -- | When a context switch is performed, this value is set with a continuation to be called after
  -- the 'changeRootHapplet' function completes. After evaluation of the 'GUI' function completes,
  -- the state is evaluated -- every single event handler will evaluate this function. To ensure
  -- that nothing happens unless it is set, this lens is used to set the callback to @return ()@ (a
  -- no-op) prior to evaluation of the 'GUI' procedure in the 'providerLiftGUI' function.
  contextSwitcher :: Lens' provider (m (EventHandlerControl ()))

  -- | When initializing a new 'provider' data structure, it must be initailized within a
  -- continuation function "initializer" that is evaluated by the 'initProviderState'. The
  -- 'initProviderState' function will pass a 'ProviderStateLock' as an argument to the initializer
  -- continuation, and this 'ProviderStateLock' must be stored somewhere in the @provider@ data
  -- structure. The 'providerSharedState' (this function) must retrieve the 'ProviderStateLock' that
  -- was stored into the @provider@ data structure by the initializer.
  --
  -- This function is only called internally to the 'GUI' monad, it must never be called anywhere
  -- else. Do NOT evaluate 'runProviderOnLock' with the result of this function, it is undefined
  -- behavior, but will very likely result in the program deadlocking.
  providerSharedState :: provider -> ProviderStateLock provider

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

-- | This function should only ever be used by Happlet providers when constructing a value of type
-- 'Happlets.Provide.Provider', or when implementing an instance of 'providerRunSync' function. The
-- default implementation of 'providerRunSync' does call this function.
--
-- The 'installEventHandler' function evaluates this function automatically while evaluating an
-- event handler callback continuation.
--
-- This thread acquires a lock on the 'ProviderStateLock', so when evaluating this function you be
-- absolutely certain that either of the two following conditions are true:
--
-- 1. either this function is only ever evaluated in thread that is NOT the main GUI event handler
--    loop (the loop which listenes for requests from the operating system and dispatches them to
--    the event handler callbacks). This function is synchronous and have to wait for the main GUI
--    event handler loop to relenquish the lock on the 'ProviderStateRef' before the function
--    continuation function @m a@ can be evaluated...
--
-- 2. or, the main GUI event handler loop releases the 'ProviderStateLock' before dispatching to all
--    of the continuation functions to be evaluated in it's own thread, as each time a continuation
--    is evaluated the 'ProviderStateLock' is re-acquired for the duration of that continuation's
--    evaluation.
--
runProviderOnLock
  :: (MonadIO m, MonadProvider provider m)
  => ProviderStateLock provider -> m a -> IO a
runProviderOnLock (ProviderStateLock lock) f = modifyMVar' lock $ runProviderIO f

-- | This function should be used to make stateful updates to the @provider@.
liftGUIProvider
  :: (MonadIO m, MonadState provider m, MonadProvider provider m)
  => m a -> GUI provider model a
liftGUIProvider f = do
  guist <- getGUIState
  (a, provider) <- liftIO $ runProviderIO f $ theGUIProvider guist
  putGUIState $ guist{ theGUIProvider = provider }
  return a

-- | This function evaluates 'GUI' functions within event handlers. This function will then lock the
-- 'Happlet' and then evaluate the 'GUI' function. The 'EventHandlerControl' returned indicates how
-- the evaluation of the 'GUI' function was terminated.
providerLiftGUI
  :: (MonadIO m, MonadState provider m, MonadProvider provider m, ProviderSyncCallback provider m)
  => Happlet model -> GUI provider model a -> m (EventHandlerControl a)
providerLiftGUI happlet f = do
  -- (1) Always set 'contextSwitcher' to a no-op, it may be set to something else by @f@.
  contextSwitcher .= return (EventHandlerContinue ())
  -- (2) Get the provider after having set it's 'contextSwitcher' to a no-op.
  provider <- get
  -- (3) Evaluate the continuation, which will update both the provider state and the Happlet state.
  (result, provider) <- liftIO $ fmap fst $ onHapplet happlet $ \ model -> do
    (guiContin, guiState) <- runGUI f GUIState
      { theGUIHapplet  = happlet
      , theGUIProvider = provider
      , theGUIModel    = model
      }
    return ((guiContin, theGUIProvider guiState), theGUIModel guiState)
  -- (4) Put the newly updated provider state back into the monad state
  put provider
  -- (5) Check if the result is 'HappletEventCancel', if it is, there is a chance that a context
  -- switch occurred. The context switcher is always evaluated on a cancel signal. If the context
  -- switcher was not changed by @f@ between (1) and (2), the 'contextSwitcher' field has therefore
  -- not been changed from the no-op value set at (1), then no context switch occurs. (See the
  -- 'changeRootHapplet' function for the mechanism we are actuating here).
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
  :: (MonadIO m, MonadProvider provider m, ProviderSyncCallback provider m)
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
  provider <- get
  size <- getProviderWindowSize
  (>>= state . const) $ liftIO $ fmap fst $ onHapplet newHapp $ \ model -> do
    (guiContin, guist) <- runGUI (init size) GUIState
      { theGUIProvider = provider
      , theGUIHapplet  = newHapp
      , theGUIModel    = model
      }
    return ((guiContin, theGUIProvider guist), theGUIModel guist)

----------------------------------------------------------------------------------------------------

-- | This data structure allows a Happlet provider implementation to specify the continuations to be
-- called by any and all event handling callback functions. You pass a value of this data type to
-- 'installEventHandler'.
--
-- A default 'EventSetup' data type can be constructed with a lens and an event handler continuation
-- using the 'simpleSetup' function.
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
      -- system executive.
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
  :: (MonadIO m, MonadProvider provider m, ProviderSyncCallback provider m)
  => EventSetup provider m event model -> GUI provider model ()
installEventHandler setup = do
  happ <- askHapplet
  liftGUIProvider $ do
    forceDisconnect (eventLensReaction setup)
    eventPreInstall setup
    provider <- get
    disconnect <- liftIO $ eventInstall setup provider $
      -- Here we install an IO function into a callback. As soon as this callback is executed, the
      -- provider must be locked using 'runProviderOnLock' in order to begin evaluating the 'GUI'
      -- function associated with this event callback.
      runProviderOnLock (providerSharedState provider) . evalConnectReact (eventLensReaction setup)
    eventLensReaction setup .= ConnectReact
      { doDisconnect = liftIO disconnect
      , reactTo =
          providerLiftGUI happ . eventGUIReaction setup >=>
          checkGUIContinue (eventLensReaction setup)
      }

----------------------------------------------------------------------------------------------------

-- | This typeclass provides an abstraction for installing continuation callback functions to be
-- evaluated in a cooperative multi-tasking system. In these systems, the main thread (sometimes the
-- only thread) runs in a tight loop listening for input events. On each cycle of the loop, updates
-- the the global @provider@ state are constantly being made. If another thread were to try to
-- update this global @provider@ state without locking, race conditions will occur. So instead,
-- cooperative multi-tasking frameworks let you set a hook function that can be run in sequence with
-- other global state updates in the main thread, so no race conditions occur.
--
-- The Happlets framework offers arbitrary use of Haskell threads. In order for Haskell threads to
-- update the global state, a thread has to be able to set a hook that locks the global @provider@
-- state using 'runProviderOnLock', and makes updates to it. The Haskell thread must block
-- until the hook is executed in the main thread.
--
-- Providers overriding this 'providerRunSync' function must take an action function of type @m()@
-- and set a hook that evaluates it in the main thread, and this function must block until the hook
-- is evaluated.
--
-- __NOTE:__ that this function __MUST__ evaluate the continuation given here using
-- 'runProviderOnLock'. So providers must make sure that the 'ProviderStateLock' is released by the
-- main GUI event loop thread before it executes a call to the continuation function given here.
--
-- For providers in which the global @provider@ state is already thread safe without needing a
-- cooperative multi-tasking mechanism, the default implementation of this function is thread safe,
-- and makes the necessary call to 'runProviderOnLock'.
class (MonadIO m, MonadProvider provider m) =>
  ProviderSyncCallback provider m | m -> provider where
    -- | This function is called by 'forkGUI', it should only be instantiated by Happlet providers.
    -- This function otherwise must never be used anywhere else.
    providerRunSync :: ProviderStateLock provider -> m a -> IO a
    providerRunSync = runProviderOnLock

-- | Functions of this type are created by the 'forkGUI' function and passed as an argument to the
-- function that will be evaluated in a separate parallel thread. Functions of this type take a
-- signal function of type 'GUI'. The 'SendGUISignal' function sends the 'GUI' function as a signql
-- to be evaluated in the 'GUI' @provider@ and @model@ context that created this function.
type SendGUISignal provider model a = GUI provider model a -> IO (EventHandlerControl a)

-- | 'forkGUI' is a good way to create threads that block while waiting for some external event, for
-- example to receive some information on a socket, and then when the event occurs, it can call the
-- given 'SendGUISignal' to send it's own event signal that performs some update on the 'GUI'.
--
-- When the 'forkGUI' function is evaluated in a 'GUI' function context (let's call this the
-- "parent" context) it will keep a reference to the @'Happlet' model@ and @ProviderStateLock
-- provider@ of the parent 'GUI' context. Then, 'forkGUI' will do two things:
--
-- 1. create a 'SendGUISignal' function which can be evaluated an unlimited number of times, and
--
-- 2. calls 'forkIO' to create a new parallel thread of computation (let's call this the "child"
--    thread) which evaluates independently or in parallel with the parent context thread.
-- 
-- The child thread will evaluate a continuation function of type @IO ()@ that you (the programmer)
-- provide to it. This continuation could, for example, be used to make observations or stateful
-- changes to the operating system environment. The child thread can make updates to the @model@ and
-- @provider@ of the parent context by sending a 'GUI provider model' function as a signal via the
-- 'SendGUISignal' function that is given to your continuation.
--
-- Every time a 'GUI' function is sent, it is evaluated in the parent 'GUI' function context,
-- possibly causing an update in the @model@ and @provider@. The 'SendGUISignal' is __synchronous__
-- so it will block until the signal is dispatched and a result is returned. The result of the
-- 'SendGUISignal' function, when it returns, will be an 'EventHandlerControl' value indicating the
-- success/failure status of the 'GUI' function that was sent as a signal.
--
-- __Note for providers:__
--
-- For providers that provide true multi-threading for event dispatch, the 'SendGUISignal' function
-- actually evaluates the 'GUI' signal in the child thread, rather than actually sending the 'GUI'
-- function off to a dispatcher to be evaluated in a different thread, and this will block when the
-- lock on the 'GUI' state is obtained.
-- 
-- However some providers, notably providers with cooperative multi-tasking, rather than true
-- multi-threading, will not actually allow you to make modifiations to the GUI's internal state in
-- a separate thread. For these providers, what 'forkGUI' will actually do is create a new Haskell
-- thread (using 'forkIO', which creates a ligthweight thread in Haskell's runtime system), and the
-- resultant 'SendGUISignal' function will actually send the 'GUI' function you give it here
-- synchronously to the task dispatcher in the GUI provider to be evaluated. Since it is
-- synchronous, the Haskell thread will block until the GUI task dispatcher fully evalutes the 'GUI'
-- function. The task dispatcher prevents race conditions by ensuring that only one task can update
-- the GUI's internal state at a time, by running only one task at a time, so it is really not
-- parallel function evaluation at all.
--
-- However, the Haskell thread created that blocks on the GUI provider's task dispatcher will not
-- block other Haskell threads, so as long as the Happlet application is compiled with the
-- @-threaded@ flag, there can still be true multi-threading in the application (e.g. while waiting
-- for messags on a socket) even if there is no multi-threading in the GUI.
forkGUI
  :: ProviderSyncCallback provider m
  => (SendGUISignal provider model a -> IO ())
  -> GUI provider model ThreadId
forkGUI child = do
  guist <- getGUIState
  liftIO $ forkIO $ child $
    providerRunSync (providerSharedState $ theGUIProvider guist) .
    providerLiftGUI (theGUIHapplet guist)
