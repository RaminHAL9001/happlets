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
    forkGUI, bracketGUI,

    -- ** The Display Typeclass
    Display(..), DrawableRef(..), drawableEmptyRef,

    -- * Functions for Providers
    EventHandlerControl(..), MonadProvider(..),
    EventSetup(..), simpleSetup, installEventHandler,

    -- ** Multi-Threading
    CanRecruitWorkers(..), ProviderStateRef, initProviderState,
    liftGUIProvider, otherThreadRunProviderIO,

    --ProviderStateLock, runProviderOnLock, providerLiftGUI, checkGUIContinue,
    --GUIState(..), guiModel, guiProvider, guiIsLive, execGUI, runGUI, guiCatch,
    --getGUIState, putGUIState, modifyGUIState, askHapplet, govWorkerUnion,

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
    , theGUIProvider :: !(ProviderStateLock provider)
--  , theGUIWorkers  :: !WorkerUnion
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

---- | Like 'runGUI' but discards the @a@ typed return value. This is analogous to
---- 'Control.Monad.State.execStateT' in the "Control.Monad.State" module.
--execGUI
--  :: GUI provider model a -- ^ the 'GUI' function to evaluate
--  -> GUIState provider model
--  -> IO (GUIState provider model)
--execGUI f = fmap snd . runGUI f

---- | Evaluate a 'GUI' function but catch calls to 'cancelNow', 'deleteEventHandler', and
---- 'Control.Monad.Fail.fail'.
--guiCatch
--  :: GUI provider model a
--  -> (EventHandlerControl a -> GUI provider model b)
--  -> GUI provider model b
--guiCatch (GUI f) = ((GUI $ f >>= \ result -> return (EventHandlerContinue result)) >>=)

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
  .  (MonadProvider provider m, CanRecruitWorkers provider m)
  => Happlet newModel -> (PixSize -> GUI provider newModel ()) -> GUI provider oldModel ()
changeRootHapplet newHapp init = do
  -- Here we set the 'contextSwithcer' field to a function that evaluates 'switchContext'.
  modifyGUIState $ \ guist -> case guist ^. guiProvider of
    ThisThreadCanTryToLock{} -> error "changeRootHapplet was evaluated on an unlocked provider state"
    ThisThreadHasTheLock env -> guist & guiProvider .~
      ( ThisThreadHasTheLock
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
newtype ProviderStateRef provider = ProviderStateRef (MVar provider)
  deriving Eq

-- | This function creates a new 'ProviderStateRef', and then evaluates a continuation which must
-- initialize the @provider@ and return it. A reference to the 'ProviderStateRef' is passed to the
-- continuation, it is expected that this reference be stored into the @provider@ data structure so
-- that it can be retrieved with the 'providerSharedState' function.
initProviderState :: (ProviderStateRef provider -> IO provider) -> IO (ProviderStateRef provider)
initProviderState init = do
  mvar <- newEmptyMVar
  let ref = ProviderStateRef mvar
  init ref >>= putMVar mvar
  return ref

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
  providerSharedState :: provider -> ProviderStateRef provider

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
  = ThisThreadCanTryToLock (ProviderStateRef provider)
  | ThisThreadHasTheLock   provider
    -- ^ This means that this current thread can modify the @provider@ state freely. This thread is
    -- within 'modifyMVar' continuation of the 'MVar' that contains the @provider@ state value and
    -- can modify it freely, and hopefully very quickly.

-- | Lock a 'ProviderStateLock' and then evaluate 'runProviderIO' on the given 'MonadProvider'
-- function @m@.
runProviderOnLock
  :: (MonadIO m, MonadProvider provider m)
  => ProviderStateLock provider -> m a -> IO a
runProviderOnLock lock f = case lock of
  ThisThreadCanTryToLock ref -> otherThreadRunProviderIO f ref
  ThisThreadHasTheLock{}     -> fail $ "runProviderOnLock: evaluated on already locked provider."

-- | This function should always be called within a callback function that is installed into the
-- provider for handling events coming into the program from the operating system.
liftGUIProvider
  :: (MonadIO m, MonadState provider m, MonadProvider provider m)
  => m a -> GUI provider model a
liftGUIProvider f = getGUIState >>= \ gui -> case theGUIProvider gui of
  ThisThreadCanTryToLock{} -> error $ "liftGUIProvider: " ++
    "Evaluated a provider function within a GUI function on a locked ProviderStateLock."
  ThisThreadHasTheLock env -> do
    (a, env) <- liftIO $ runProviderIO f env
    putGUIState $ gui{ theGUIProvider = ThisThreadHasTheLock env }
    return a

-- | This function evaluates 'GUI' functions within event handlers. It evaluates to a function of
-- type @m@ (which must be a stateful function that has a @provider@ as part of it's state), which
-- means you are required to have first evaluated 'runProviderOnLock'. This function will then lock
-- the 'Happlet' and then evaluate the 'GUI' function. The 'EventHandlerControl' returned indicates
-- how the evaluation of the 'GUI' function was terminated.
providerLiftGUI
  :: (MonadIO m, MonadState provider m, MonadProvider provider m, CanRecruitWorkers provider m)
  => Happlet model -> GUI provider model a -> m (EventHandlerControl a)
providerLiftGUI happlet f = do
  -- (1) Always set 'contextSwitcher' to a no-op, it may be set to something else by @f@.
  contextSwitcher .= return (EventHandlerContinue ())
  -- (2) Get the provider after having set it's 'contextSwitcher' to a no-op.
  provider <- get
  (result, provider) <- liftIO $ fmap fst $ onHapplet happlet $ \ model -> do
    (guiContin, guiState) <- runGUI f GUIState
      { theGUIHapplet  = happlet
      , theGUIProvider = ThisThreadHasTheLock provider
      , theGUIModel    = model
--    , theGUIWorkers  = governmentWorkerUnion provider
      }
    case theGUIProvider guiState of
      ThisThreadHasTheLock provider -> return ((guiContin, provider), theGUIModel guiState)
      ThisThreadCanTryToLock{}      -> fail $
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
  :: (MonadIO m, MonadProvider provider m, CanRecruitWorkers provider m)
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
      { theGUIProvider = ThisThreadHasTheLock env
      , theGUIHapplet  = newHapp
      , theGUIModel    = model
--    , theGUIWorkers  = governmentWorkerUnion env
      }
    case theGUIProvider guist of
      ThisThreadHasTheLock winst -> return ((guiContin, winst), theGUIModel guist)
      ThisThreadCanTryToLock{}   -> fail $
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
  :: (MonadIO m, MonadProvider provider m, CanRecruitWorkers provider m)
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
      runProviderOnLock (ThisThreadCanTryToLock $ providerSharedState env) .
      evalConnectReact (eventLensReaction setup)
    eventLensReaction setup .= ConnectReact
      { doDisconnect = liftIO disconnect
      , reactTo =
          providerLiftGUI happ . eventGUIReaction setup >=>
          checkGUIContinue (eventLensReaction setup)
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
-- of time to run to completion, call 'providerForkGUI' which creates a thread-safe evaluator for
-- functions of type 'GUI' that can be safely evaluated in the @IO@ context of the thread.
--
-- It should be noted that these APIs are modeled after the Glib Haskell library, which has
-- abstractions for programming single-threaded, cooperative multi-tasking platforms.
class (MonadIO m, MonadProvider provider m) => CanRecruitWorkers provider m | m -> provider where
--  governmentWorkerUnion :: provider -> WorkerUnion

  -- | This function provides an abstraction for cooperative multi-tasking. In these systems, the
  -- main thread (sometimes the only thread) runs in a tight loop listening for input events. On
  -- each cycle of the loop, updates the the global @provider@ state are constantly being made. If
  -- another thread were to try to update this global @provider@ state without locking, race
  -- conditions will occur. So instead, cooperative multi-tasking frameworks let you set a hook
  -- function that can be run in sequence with other global state updates in the main thread, so no
  -- race conditions occur.
  --
  -- The Happlets framework offers arbitrary use of Haskell threads. In order for Haskell threads to
  -- update the global state, a thread has to be able to set a hook that locks the global @provider@
  -- state using 'otherThreadRunProviderIO', and makes updates to it. The Haskell thread must block
  -- until the hook is executed in the main thread.
  --
  -- Providers overriding this 'providerRunSync' function must take an action function of type @m()@
  -- and set a hook that evaluates it in the main thread, and this function must block until the
  -- hook is evaluated.
  --
  -- __NOTE:__ that this function __MUST__ evaluate the continuation given here using
  -- 'otherThreadRunProviderIO', which must obtains the lock on the 'ProviderStateRef' given by
  -- 'providerSharedState'.
  --
  -- For providers in which the global @provider@ state is already thread safe without needing a
  -- cooperative multi-tasking mechanism, the default implementation of this function is thread
  -- safe, and makes the necessary call to 'otherThreadRunProviderIO'.
  providerRunSync :: m () -> m ()
  providerRunSync f = do
    shared <- gets providerSharedState
    liftIO (otherThreadRunProviderIO (f >> get) shared) >>= put

-- | This thread acquires a lock on the 'ProviderStateRef', so be absolutely certain that this
-- function is only ever evaluated in a separate thread.
otherThreadRunProviderIO
  :: MonadProvider provider m
  => m a -> ProviderStateRef provider -> IO a
otherThreadRunProviderIO f (ProviderStateRef mvar) = modifyMVar' mvar $ runProviderIO f

-- This function must be run on 'GUIState' containing a locked 'ProviderStateLock'. Using this
-- provider state, the 'providerRunSync' function is evaluated, which in turn evaluates the given
-- 'GUI' function in a different thread.
--
-- Before evaluating the 'GUI' function, 'providerRunSync' is required to use 'providerSharedState'
-- to obtain the same 'ProviderStateRef' that would be extracted from an unlocked
-- 'ProviderStateLock'. Then 'providerRunSync' sends to another thread a function which uses
-- 'otherThreadRunProviderIO' to evaluate the 'ProviderStateRef' that it obtained from
-- 'providerSharedState', and finally, within 'otherThreadRunProviderIO', the 'providerLiftGUI'
-- function is used to evaluate the 'GUI' continuation function was passed here to
-- 'inOtherThreadRunGUI'.
--
-- So this function does operate on an unlocked 'ProviderStateLock', but does not modify it, rather
-- it uses the 'ProviderStateRef' within to creates a new function and sends this new function to
-- 'providerRunSync' which then does perform locking on the 'ProviderStateRef' and evaluates the
-- new function which evaluates the given 'GUI' function using 'providerLiftGUI'.
--
-- The 'providerRunSync' is responsible for updating the MVar in the 'providerSharedState', and the
-- 'providerLiftGUI' function is responsible for updating the 'GUIState', so this function discards
-- both the 'providerSharedState' and the 'GUIState' that it receives after evaluating this
-- function, because it is not the responsibility of this function to update those MVars.
inOtherThreadRunGUI
  :: CanRecruitWorkers provider m
  => GUIState provider model -> GUI provider model a -> IO ()
inOtherThreadRunGUI guist f = case theGUIProvider guist of
  ThisThreadHasTheLock prost -> fmap fst $ flip runProviderIO prost $
    providerRunSync $ void $ providerLiftGUI (theGUIHapplet guist) f
  ThisThreadCanTryToLock{}   -> error
    "inOtherThreadGUI: was given GUIState containing an unlocked window."

-- | Works similar to 'forkIO' but for the 'GUI' function type, with one very big caveat:
--
-- Some providers, notably providers with cooperative multi-tasking, rather than true
-- multi-threading, will not actually allow you to make modifiations to the GUI's internal state in
-- a separate thread. For these providers, what 'forkGUI' will actually do is create a new Haskell
-- thread (using 'forkIO', which creates a ligthweight thread in Haskell's runtime system), and this
-- thread will actually send the 'GUI' function you give it here synchronously to the task
-- dispatcher in the GUI provider to be evaluated. Since it is synchronous, Haskell thread will
-- block until the GUI task dispatcher fully evalutes the 'GUI' function. The task dispatcher
-- prevents race conditions by ensuring that only one task can update the GUI's internal state at a
-- time, by running only one task at a time, so it is really not parallel function evaluation at
-- all.
--
-- However, the Haskell thread created that blocks on the GUI provider's task dispatcher will not
-- block other Haskell threads, so if the Happlet application is compiled with the @-threaded@ flag,
-- there can still be true multi-threading in the application, just not multiple updates to the GUI
-- at once.
forkGUI
  :: CanRecruitWorkers provider m
  => GUI provider model () -> GUI provider model ThreadId
forkGUI f = do
  st <- getGUIState
  liftIO $ forkIO $ void $ inOtherThreadRunGUI st f
