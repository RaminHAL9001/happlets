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
-- The "Happlets.Event" module provides a consistent API for all back-end
-- 'Happlets.Provider.Provider's, but the subset of which event data types that your 'Happlet'
-- program will be able to respond to depends on the back-end "Happlets.Provider" you are using and
-- which type classes the @window@ type instantiates. The type classes that provide the API
-- functions for installing 'GUI' event handlers include:
--
--  * 'Managed'
--  * 'CanResize'
--  * 'CanAnimate'
--  * 'CanMouse'
--  * 'CanKeyboard'
--  * 'CanTrackpad'
--  * 'CanForceFeedback'
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
module Happlets.GUI
  ( -- * The Happlet Data Type
    Happlet, HappletWindow(..), Display(..), getWaitingThreads,

    -- * The GUI Function Type
    GUI, getModel, getSubModel, putModel, modifyModel,
    cancelNow, cancelIfBusy, howBusy, deleteEventHandler, bracketGUI,

    -- * Widgets
    Widget, widget, widgetState, widgetBoundingBox, onWidget, mouseOnWidget,
    widgetContainsPoint, widgetContainsMouse, theWidgetState,

    -- * Installing Event Handlers
    -- $InstallingEventHandlers
    Managed(..),
    CanRecruitWorkers(..),
    CanResize(..), OldPixSize, NewPixSize, CanvasMode(..),
    CanAnimate(..),
    CanMouse(..), MouseEventPattern(..),
    CanKeyboard(..),
    CanTrackpad(..),

    -- * Buffered Images
    CanBufferImages(..),

    -- * Workers
    -- $Workers
    Workspace, newWorkspace, copyWorkspace,
    WorkerName, guiWorker, recruitWorker, relieveWorker, relieveWorkersIn,
    WorkPost, postWorkersUnion, pushWork, checkWork,
    postWorkers, postTaskWorkers, guiPostWorkers, closeWorkPost,
    WorkPostStats(..), getWorkPostStats, resetWorkPostStats, instantThroughput, totalThroughput,
    WorkerTask,  govWorker, taskPause, thisWorker, taskDone, taskSkip, taskFail, taskCatch,
    Worker, workerThreadId, workerName, relieveGovWorkers,
    WorkerStatus(..), getWorkerStatus, workerNotBusy, workerNotDone, workerBusy, workerFailed,
    WorkerUnion, newWorkerUnion,
    WorkerSignal(..),

    -- * Other Capabilities
    -- $OtherCapabilities
    CanForceFeedback(..),
    DeviceHandler(..), unrecognizedInputDevices, failUnrecognizedInputDevices,

    -- * Low-level details
    -- $LowLevel_Details
    GUIState(..), EventHandlerControl(..),
    guiModel, guiWindow, guiIsLive, execGUI, runGUI, guiCatch,
    makeHapplet, onHapplet, peekModel, sameHapplet,
    getGUIState, putGUIState, modifyGUIState, askHapplet, govWorkerUnion,

  ) where

import           Prelude hiding ((.), id)

import           Happlets.Event
import           Happlets.Draw.SampCoord
import           Happlets.Draw.Types2D

import           Control.Arrow
import           Control.Category
import           Control.Concurrent (ThreadId, myThreadId, yield, forkIO, threadDelay, throwTo)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import qualified Control.Monad.Fail    as Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.List             (intercalate)
import           Data.Semigroup
import qualified Data.Set              as Set
import qualified Data.Text             as Strict
import           Data.Time.Clock
import           Data.Typeable

import           System.IO.Unsafe (unsafePerformIO) -- used for generating unique Happlet IDs.

----------------------------------------------------------------------------------------------------

-- | This is a handle containing the current @model@ state of the 'Happlet'. It is constructed with
-- 'makeHapplet', but it is better to use 'Happlets.Initialize.makeHapplet' in the
-- "Happlets.Initialize" module in order to construct a 'Happlet' and associate it with a @window@
-- in a single step.
data Happlet model
  = Happlet
    { happletId   :: !Int
    , happletLock :: MVar ThreadTracker
    , happletMVar :: MVar model
    }
  deriving (Eq, Typeable)

data ThreadTracker = ThreadTracker{ threadCount :: !Int, threadList :: ![ThreadId] }

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
onHapplet (Happlet{happletMVar=mvar,happletLock=lock}) f = do
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
peekModel :: Happlet model -> IO model
peekModel (Happlet{happletMVar=mvar}) = readMVar mvar

-- | Every thread that is waiting to access the @model@ of a 'Happlet' is recorded. This function
-- returns the number of threads that are waiting for access.
getWaitingThreads :: Happlet model -> IO (Int, [ThreadId])
getWaitingThreads (Happlet{happletLock=lock}) = (threadCount &&& threadList) <$> readMVar lock

modifyMVar' :: MVar st -> (st -> IO (a, st)) -> IO a
modifyMVar' mvar f = modifyMVar mvar $ fmap (\ (a, st) -> (st, a)) . f

----------------------------------------------------------------------------------------------------

-- | All Happlet back-end 'Happlets.Provider.Provider's must instantiate this type class at the very
-- least. The functions in this typeclass allow you to draw images that become visible on screen.
--
-- When drawing images, keep in mind that the Happlet window is double buffered, and you may draw to
-- either buffer.
--
-- * There is a __"canvas"__ buffer which you fully control, only your code has access to this
--   buffer. This buffer has more permanence, because the only time it can be changed is when your
--   own code changes it.
--
-- * There is also an __"OS buffer"__ which is a buffer managed by your operating system's window
--   manager. This buffer is often modified by the operating system, for example, when a window from
--   another application is laid on top of your Happlet's window.
--
-- When the OS buffer becomes "dirty" (modified by some event happening elsewhere in the operating
-- system and window manager), the canvas buffer is used to re-draw the dirty region of the window.
--
-- The only time you should draw to the __OS buffer__ is when drawing a temporary image, like a
-- custom mouse cursor, or a drag-and-drop shadow. You can draw an image that follows the mouse
-- cursor, then when the cursor moves again, use 'refreshWindow' or 'refreshRegion' to delete the
-- temporary image and redrawing that part of the window with the __canvas__ buffer, and draw the
-- cursor again at the new mouse location.
class HappletWindow window render | window -> render where

  -- | Return the size of the window. As you can see from the type of this function, it can only be
  -- evaluated from within a GUI, which guarantees the window size is valid for the duration of the
  -- function evaluation. After a 'GUI' function finishes evaluation, it is not called again until a
  -- new event comes in, at which time the size of the window may have changed and you will need to
  -- call this function again to obtain the new size. It is a good idea to call this function first
  -- before doing anyting else, if you need the window size for your 'GUI' computation.
  getWindowSize :: GUI window model PixSize

  -- | Similar to 'doWindowNewHapplet', except places an existing 'Happlet' into the @window@,
  -- removing the previous 'Happlet'. This is effectivel a context switch that occurs within a
  -- single @window@. This function disables all event handlers, then evaluates the given 'GUI'
  -- function which should install new event handlers. This function then evaluates to 'disable', so
  -- any line of code in the @do@ block written after this function is called will never execute.
  windowChangeHapplet
    :: forall newmodel oldmodel . Happlet newmodel
    -> (OldPixSize -> GUI window newmodel ())
    -> GUI window oldmodel ()

  -- | This event handler is called when 'windowChangeHapplet' is called, allowing one final event
  -- handler to be called for cleaning-up, before the current 'Happlet' is detached from the
  -- @window@.
  changeEvents :: GUI window oldmodel () -> GUI window oldmodel ()

  -- | Push a clip region of the window's canvas buffer (the buffer that is drawn to by the
  -- 'onCanvas' function). The coordinates used by all drawing operations will be translated such
  -- that the upper-left corner of the 'Happlets.Draw.Types2D.Rect2D' within the window will be the
  -- point @(0, 0)@ from the perspective of the 'onCanvas' @render@ing function. Only the top
  -- element of the clip region stack is used, the remainder of the stack is ignored.
  pushWindowClipRegion :: Rect2D SampCoord -> GUI window model ()

  -- | Pop the clip reigion of the window's canvas buffer that was last pushed by
  -- 'setWindowClipRegion'. If the stack of clip regions is empty, drawing operations now operate on
  -- the entire window.
  popWindowClipRegion :: GUI window model ()

  -- | Construct a 'GUI' function which evaluates a @render@ function that updates the happlet's own
  -- "canvas", which is an image buffer accessible only to your happlet, (it serves as the invisible
  -- half of the double-buffer renderer). The Images drawn with this function are automatically
  -- copied back to the operating system's window buffer (the visible half of the double-buffer
  -- renderer) whenever the window becomes "dirty" from events happening outside of the program,
  -- like when a your Happlet's window is covered by another window from another application.
  --
  -- As a result, it is usually best to draw to this buffer if you want an image that isn't effected
  -- when you switch away from your Happlet to another applcation window, images drawn to the canvas
  -- have a bit more permenance to them, since only your happlet can change it.
  --
  -- __NOTE__ that when a window is resized, the canvas buffer is deleted and a new one is allocated
  -- to match the requested window size, so drawing to the canvas does not preserve your image if
  -- your window is resized, you must still redraw after the window is resized.
  onCanvas :: forall model a . render a -> GUI window model a

  -- | Similar to 'onCanvas' but does NOT draw to the Happlet's own reserved image buffer (the
  -- invisible side of the double-buffer renderer), rather it draws directly to the image buffer
  -- used by the operating system (the visible side of the double-buffer renderer).
  --
  -- There are a few consequences to drawing directly to the OS buffer, the first being that the OS
  -- image buffer is regularly over-written by other applications running in the operating system,
  -- so things drawn to the OS image buffer may dissapear at any time, even when there aren't any
  -- events passed to your Happlet.
  --
  -- Another consequence is that you can overwrite the OS buffer with your happlet's "canvas" buffer
  -- at any time using 'refreshRegion'. This can be very useful when you want a temporary image to
  -- be displayed on top of your Happlet (like a pop-up menu or custom mouse cursor) that can be
  -- erased and replaced with the content of the "canvas" buffer as soon as (for example) the mouse
  -- moves or some such event occurs.
  onOSBuffer :: forall model a . render a -> GUI window model a

  -- | Force an area of the window to be redrawn by re-blitting the double buffer image to the
  -- window. Use this method to clear parts of the window that have been drawn over by the
  -- 'drawToWindow' function.
  refreshRegion :: [Rect2D SampCoord] -> GUI window model ()

  -- | Like 'refreshRegion' but refreshes the whole window. This function deletes everything drawn
  -- by the 'drawToWindow' function and replaces it with the content of the image drawn by
  -- 'onCanvas'.
  refreshWindow :: GUI window model ()

-- | This is a type class similar to the 'Prelude.Show' class, except it displays to the 'GUI'.
class Display drawable render | drawable -> render where
  display :: HappletWindow window render => drawable -> GUI window model ()

----------------------------------------------------------------------------------------------------

-- | A widget is any part of your @model@ that can be drawn to the @window@ that is restricted to a
-- rectangular bounding box. Use the 'onWidget' function to evaluate stateful updates on the
-- 'widgetState'
data Widget st
  = Widget
    { theWidgetBoundingBox :: !(Rect2D SampCoord)
    , theWidgetState       :: st
      -- ^ a function that accesses the 'Widget' state without need for the 'Control.Lens.view'
      -- function.
    }

-- | Construct a new 'Widget'.
widget :: st -> Rect2D SampCoord -> Widget st
widget = flip $ Widget . canonicalRect2D

-- | The state of this widget. The model of a widget should contain values that are directly related
-- to elements that are drawn to the happlet window. Keeping part of the @model@ data structure is
-- bad practice.
widgetState :: Lens' (Widget st) st
widgetState = lens theWidgetState $ \ a b -> a{ theWidgetState = b }

-- | The bounding box of this widget.
widgetBoundingBox :: Lens' (Widget st) (Rect2D SampCoord)
widgetBoundingBox = lens theWidgetBoundingBox $ \ a b ->
  a{ theWidgetBoundingBox = canonicalRect2D b }

-- | Does a 'PixCoord' lie somewhere within the 'widgetBoundingBox'?
widgetContainsPoint :: PixCoord -> Widget st -> Bool
widgetContainsPoint (V2 x y) widget = loX <= x && x <= hiX && loY <= y && y <= hiY where
  (Rect2D (V2 loX loY) (V2 hiX hiY)) = theWidgetBoundingBox widget

-- | Does a 'Mouse' event's 'Happlets.Draw.SampCoord.PixCoord' lie within the 'widgetBoundingBox'?
-- If not return 'Nothing', but if so, modify the 'Mouse' event so that it's coordinates are
-- relative to the 'widgetBoundingBox'.
widgetContainsMouse :: Mouse -> Widget st -> Maybe Mouse
widgetContainsMouse (Mouse dev press mods button p@(V2 (SampCoord x) (SampCoord y))) widget = do
  guard $ widgetContainsPoint p widget
  let (Rect2D (V2 (SampCoord loX) (SampCoord loY)) _) = theWidgetBoundingBox widget
  pure $ Mouse dev press mods button $ V2 (SampCoord $ x - loX) (SampCoord $ y - loY)

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
-- then call functions like 'onCanvas' to re-draw relavent parts of the @window@.
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
-- not just Gtk+, then your entire program should be written with a polymorphic @window@ type.  The
-- @window@ type is the type specific to the operating system and the Happlet provider you use, so
-- if you leave it blank, you will be free to call 'simpleHapplet' with any
-- 'Happelt.Provider.Provider' at all. However, at this time, the Happlets API is a little too
-- sparse for this to be practical, for the time being, your program may need to rely on
-- functionality specific to a Provider like Gtk+.
newtype GUI window model a
  = GUI{ unwrapGUI :: StateT (GUIState window model) IO (EventHandlerControl a)}
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
data GUIState window model
  = GUIState
    { theGUIModel   :: !model
    , theGUIHapplet :: !(Happlet model)
    , theGUIWindow  :: !window
    , theGUIWorkers :: !WorkerUnion
    }

instance Applicative (GUI window model) where { pure = return; (<*>) = ap; }

instance Monad (GUI window model) where
  return = GUI . return . EventHandlerContinue
  (GUI f) >>= next = GUI $ StateT $ \ st -> runStateT f st >>= \ (a, st) -> case a of
    EventHandlerContinue a -> runStateT (unwrapGUI $ next a) st
    EventHandlerHalt       -> return (EventHandlerHalt    , st)
    EventHandlerCancel     -> return (EventHandlerCancel  , st)
    EventHandlerFail   err -> return (EventHandlerFail err, st)

instance MonadIO (GUI window model) where
  liftIO f = GUI $ liftIO $ EventHandlerContinue <$> f

instance MonadState model (GUI window model) where
  state f = GUI $ state $ \ st ->
    let (a, model) = f (theGUIModel st) in (EventHandlerContinue a, st{ theGUIModel = model })

instance MonadReader (Happlet model) (GUI window model) where
  ask = GUI $ EventHandlerContinue <$> gets theGUIHapplet
  local loc (GUI f) = GUI $ do
    oldst <- get
    put $ oldst{ theGUIHapplet = loc $ theGUIHapplet oldst }
    f <* modify (\ newst -> newst{ theGUIHapplet = theGUIHapplet oldst })
  
instance Monad.MonadFail (GUI window model) where
  fail = GUI . return . EventHandlerFail . Strict.pack

instance MonadError Strict.Text (GUI window model) where
  throwError = GUI . return . EventHandlerFail
  catchError (GUI try) catch = GUI $ try >>= \ case
    EventHandlerFail msg -> unwrapGUI $ catch msg
    result               -> return result

instance Semigroup a => Semigroup (GUI window model a) where
  a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (GUI window model a) where
  mempty = return mempty
  mappend a b = mappend <$> a <*> b

-- | 'Control.Lens.Lens' for manipulating the 'GUIState'. It is better to use 'getModel',
-- 'updateModel', 'putModel', 'subModel', or 'liftGUI' instead of manipulating a 'GUIState'
-- directly.
guiModel :: Lens' (GUIState window model) model
guiModel = lens theGUIModel $ \ a b -> a{ theGUIModel = b }

-- | 'Control.Lens.Lens' for manipulating the 'GUIState'. It is better to not use this function at
-- all.
guiWindow :: Lens' (GUIState window model) window
guiWindow = lens theGUIWindow $ \ a b -> a{ theGUIWindow = b }

-- | Evaluate a 'GUI' function on a given 'Happlet'. The 'Happlet' is __NOT__ locked during
-- evaluation, it is expected that the 'Happlet' has already been locked with 'onHapplet', however a
-- copy of the 'Happlet' must be passed along with it's content to this function in order to
-- evaluate it. This is analogous to 'Control.Monad.State.runStateT' in the "Control.Monad.State"
-- module.
runGUI
  :: GUI window model a -- ^ the 'GUI' function to evaluate
  -> GUIState window model
  -> IO (EventHandlerControl a, GUIState window model)
runGUI (GUI f) st = runStateT f st

-- | Like 'runGUI' but discards the @a@ typed return value. This is analogous to
-- 'Control.Monad.State.execStateT' in the "Control.Monad.State" module.
execGUI
  :: GUI window model a -- ^ the 'GUI' function to evaluate
  -> GUIState window model
  -> IO (GUIState window model)
execGUI f = fmap snd . runGUI f

-- | Evaluate a 'GUI' function but catch calls to 'cancelNow', 'deleteEventHandler', and
-- 'Control.Monad.Fail.fail'.
guiCatch
  :: GUI window model a
  -> (EventHandlerControl a -> GUI window model b)
  -> GUI window model b
guiCatch (GUI f) = ((GUI $ f >>= \ result -> return (EventHandlerContinue result)) >>=)

-- | Like the 'Control.Exception.bracket' function, but works in the 'GUI' monad.
bracketGUI
  :: GUI window model open
  -> GUI window model closed
  -> GUI window model a
  -> GUI window model a
bracketGUI lock unlock f = GUI $ StateT $ \ st -> do
  mvar <- newEmptyMVar
  a    <- bracket
    (runGUI lock st >>= \ (rsrc, st) -> putMVar mvar st >> return rsrc)
    (const $ modifyMVar mvar $ fmap (\ (a,b)->(b,a)) . runGUI unlock)
    (\ case
      EventHandlerContinue{} -> modifyMVar mvar $ fmap (\ (a,b)->(b,a)) . runGUI f
      EventHandlerHalt       -> return EventHandlerHalt
      EventHandlerCancel     -> return EventHandlerCancel
      EventHandlerFail err   -> return $ EventHandlerFail err
    )
  st <- takeMVar mvar
  return (a, st)

-- | Use a 'Control.Lens.Lens'' to select a 'Widget' from the current @model@, then evaluate a 'GUI'
-- function that updates this 'Widget'. The @window@ will have it's clip region set to the
-- 'widgetBoundingBox'
onWidget
  :: HappletWindow window render
  => Lens' model (Widget st)
  -> (Widget st -> GUI window model (Widget st))
  -> GUI window model st
onWidget widget f = do
  w <- use widget
  w <- bracketGUI (pushWindowClipRegion $ theWidgetBoundingBox w) popWindowClipRegion (f w)
  assign widget w
  return $ theWidgetState w

-- | Use this function to delegate a 'Happlets.Event.Mouse' event to a 'Widget'. This function
-- automatically checks if the mouse lies within the 'widgetBoundingBox' and triggers the 'GUI'
-- function evaluation using 'onWidget'.
mouseOnWidget
  :: HappletWindow window render
  => Lens' model (Widget st)
  -> Mouse
  -> (Mouse -> Widget st -> GUI window model (Widget st))
  -> GUI window model st
mouseOnWidget widget evt f = use widget >>= \ w ->
  maybe (return $ theWidgetState w) (onWidget widget . f) (widgetContainsMouse evt w)

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
deleteEventHandler :: GUI window model void
deleteEventHandler = GUI $ return EventHandlerHalt

-- | Cancel the current event handler execution. This function is like 'disable' but does not
-- instruct the event handler to remove itself.
cancelNow :: GUI window model void
cancelNow = GUI $ return EventHandlerCancel

-- | When an event handler evaluates this function, it is voluntarily canceling the current event
-- handler execution if there are one or more other threads waiting for access to the 'Happlet'
-- @model@. This function should be evaluated after making important updates to the @model@
-- (especially ones that indicate which events have been handled), but before time-consuming drawing
-- updates have begun. This allows other threads to have access to the model and perform their own
-- drawing.
cancelIfBusy :: GUI window model ()
cancelIfBusy = howBusy >>= flip when cancelNow . (> 0)

-- | Return a value indicating how many pending threads there are waiting for their turn to access
-- the 'Happlet' @model@.
howBusy :: GUI window model Int
howBusy = GUI $ fmap EventHandlerContinue $
  gets theGUIHapplet >>= liftIO . fmap fst . getWaitingThreads

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.get'. However this function
-- is provided as synonym for the 'Control.Monad.State.Class.get' function for convenience, and
-- perhaps for better readability in your code.
getModel :: GUI window model model
getModel = get

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.gets'. However this function
-- is provided as synonym for the 'Control.Monad.State.Class.gets' function for convenience, and
-- perhaps for better readability in your code.
getSubModel :: (model -> a) -> GUI window model a
getSubModel = gets

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.put'. However this function
-- is provided as synonym for the 'Control.Monad.State.Class.put' function for convenience, and
-- perhaps for better readability in your code.
putModel :: model -> GUI window model ()
putModel = put

-- | The 'GUI' function type instantiates the 'Control.Monad.State.Class.MonadState' monad
-- transformer so you can use functions like 'Control.Monad.State.Class.modify'. However this
-- function is provided as synonym for the 'Control.Monad.State.Class.modify' function for
-- convenience, and perhaps for better readability in your code.
modifyModel :: (model -> model) -> GUI window model ()
modifyModel = modify

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
getGUIState :: GUI window model (GUIState window model)
getGUIState = GUI $ EventHandlerContinue <$> get

-- | Update the entire copy of the 'GUIState' data structure for the current context. This
-- function should never be used unless you are programming a Happlet 'Happlets.Provider.Provider'.
putGUIState :: GUIState window model -> GUI window model ()
putGUIState = GUI . fmap EventHandlerContinue . put

-- | Modify the 'GUIState' data structure for the current context. This function should never be
-- used unless you are programming a Happlet 'Happlets.Provider.Provider'
modifyGUIState :: (GUIState window model -> GUIState window model) -> GUI window model ()
modifyGUIState = GUI . fmap EventHandlerContinue . modify

-- | Obtain a reference to the 'Happlet' in which this 'GUI' function is being evaluated.
askHapplet :: GUI window model (Happlet model)
askHapplet = ask

-- | Obtain a reference to the "government" 'WorkerUnion' for this GUI.
govWorkerUnion :: GUI window model WorkerUnion
govWorkerUnion = GUI $ EventHandlerContinue <$> gets theGUIWorkers

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
    { workerThreadId :: !ThreadId
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

-- | A 'WorkPost' is an area where 'Worker's waits to recieve work. Send work to the post using the
-- 'postWork' function, and check on the result using 'postCheck' function. 'Worker's are assigned
-- to a post using 'postWorkers' or 'guiPostWorkers'. 'Worker's working in a 'WorkPost' will form
-- their own 'WorkerUnion', rather than being assigned to some other 'WorkerUnion' or the government
-- 'WorkerUnion'.
--
-- Be warned that posting too much work faster than the 'Worker's can handle it will result in a
-- "space leak", a situation in which unprocessed work piles up taking more and more memory until
-- your program is forcibly haltd by the operating system.
--
-- You can check throughput statistics on a 'WorkPost' as well.
data WorkPost inWork outWork
  = WorkPost
    { postWorkersUnion :: !WorkerUnion
    , workPostStats    :: !(MVar WorkPostStats)
    , workPostInbox    :: !(Chan inWork)
    , workPostOutbox   :: !(Chan outWork)
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

setWorkerStatus :: Worker -> WorkerStatus -> IO WorkerStatus
setWorkerStatus (Worker{workerStatus=stat}) = swapMVar stat

-- | Pause for some number of seconds. The pause time can be very small, but the smallest pause time
-- is dependent on the operating system and computer hardware. Haskell's runtime system on stock
-- computer hardware can control time to within a millionth of a second with some reliability.
taskPause :: Double -> WorkerTask work ()
taskPause t = WorkerTask $ ReaderT $ \ self -> liftIO $ do
  oldStat <- setWorkerStatus self WorkerPaused
  if t <= 0 then yield else threadDelay $ round $ t * 1000 * 1000
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

-- not for export
newtype SetWaiting = SetWaiting (IO ())
newtype SetBusy    = SetBusy    (IO ())

-- | This function starts a worker on a 'WorkerTask', returning an identifier you can use to query
-- information about the 'Worker's status. In this idylic little world, workers stay with a
-- 'WorkerUnion' until they retire, so there are no APIs for moving a 'Worker' from one
-- 'WorkerUnion' to another, you can only signal to them that their work is done using the
-- 'relieveWorker' function, at which point they happily leave.
recruitWorker
  :: MonadIO m
  => WorkerUnion -> Workspace work -> WorkerName -> WorkerTask work void -> m Worker
recruitWorker wu (Workspace mvar) handl (WorkerTask f) =
  newWorker wu handl $ \ (SetWaiting waiting) (SetBusy busy) self ->
  waiting >> modifyMVar' mvar ((>>) busy . runStateT (runReaderT f self))

-- | Similar to 'recruitWorker' but works for the "government" and operates on the current @model@
-- in of the 'GUI' function which recruits this 'Worker'. This function is also similar to
-- 'govWorker', but notice that the task to be performed is not an ordinary 'WorkerTask' function,
-- but a 'GUI' function. This allows the worker to update the GUI directly, as if there were an
-- event handler constantly responding to a stream of events being generated in a parallel process.
--
-- The 'Happlet.Provider.Provider' must instantiate 'CanRecruitWorker' in order for this function to
-- be usable -- we hope that all 'Happlet.Provider.Provider's will do this.
guiWorker
  :: CanRecruitWorkers window
  => WorkerName -> GUI window model void -> GUI window model Worker
guiWorker handl f = getGUIState >>= \ st ->
  newWorker (theGUIWorkers st) handl $ \ (SetWaiting waiting) (SetBusy busy) _self ->
  waiting >> inOtherThreadRunGUI st (liftIO busy >> f)

-- | Similar to 'recruitWorker' but works in the "government" 'WorkerUnion'.
--
-- Notice that this function must be evaluate to a 'GUI' function, unlike many of the other
-- 'Worker'-related functions here which can be evaluated to any function type of the 'MonadIO' type
-- class. The default "government" 'WorkerUnion' is only available during 'GUI' evaluation.
govWorker :: Workspace work -> WorkerName -> WorkerTask work void -> GUI window model Worker
govWorker workspace handl f =
  govWorkerUnion >>= \ gov -> recruitWorker gov workspace handl f

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
-- 'WorkPost'.
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
  postal names $ \ (SetWaiting _) (SetBusy _) self work ->
  evalStateT (runReaderT (unwrapWorkerTask $ f work) self) ()

-- | Similar to 'postWorkers' but also allows the workers access to a 'Workspace'. The content of
-- 'Workspace' can be thought of as a @fold@ed value that may be updated after each @inWork@ element
-- is recieved.
postTaskWorkers
  :: MonadIO m
  => Workspace fold -> [WorkerName]
  -> (inWork -> WorkerTask fold outWork)
  -> m (WorkPost inWork outWork)
postTaskWorkers (Workspace mvar) handls f =
  postal handls $ \ (SetWaiting waiting) (SetBusy busy) self work -> do
    waiting
    modifyMVar' mvar $ (>>) busy . runStateT (runReaderT (unwrapWorkerTask $ f work) self)

-- | Similar to 'postWorkers', but the task evaluated is of type 'GUI', allowing full access to the
-- 'GUI's state as each work item is posted.
guiPostWorkers
  :: CanRecruitWorkers window
  => [WorkerName]
  -> (inWork -> GUI window model outWork)
  -> GUI window model (WorkPost inWork outWork)
guiPostWorkers names f = getGUIState >>= \ st ->
  postal names $ \ (SetWaiting waiting) (SetBusy busy) _self work -> do
    waiting
    inOtherThreadRunGUI st (liftIO busy >> f work)

-- not for export
postal
  :: MonadIO m
  => [WorkerName]
  -> (SetWaiting -> SetBusy -> Worker -> inWork -> IO (EventHandlerControl outWork))
  -> m (WorkPost inWork outWork)
postal names f = liftIO $ do
  stats  <- newWorkPostStats >>= newMVar
  inbox  <- newChan
  outbox <- newChan
  unmvar <- newMVar Set.empty
  let un   = WorkerUnion unmvar
  let post = WorkPost
        { postWorkersUnion = WorkerUnion unmvar
        , workPostStats    = stats
        , workPostInbox    = inbox
        , workPostOutbox   = outbox
        }
  let make handl = newWorker un handl $ \ sw@(SetWaiting waiting) sb@(SetBusy busy) self -> do
        work <- waiting >> readChan inbox
        postStats post
          $ (_workPostWaitingInput %~ subtract 1)
          . (_workPostTotalInput   %~ (+ 1))
          . (_workPostRecentInput  %~ (+ 1))
        result <- busy >> f sw sb self work
        case result of
          EventHandlerContinue result -> do
            writeChan outbox result
            postStats post
              $ (_workPostWaitingOutput %~ (+ 1))
              . (_workPostTotalOutput   %~ (+ 1))
              . (_workPostRecentOutput  %~ (+ 1))
          _                           -> return ()
        return result
  Set.fromList <$> mapM make names >>= swapMVar unmvar
  return post

-- not for export
newWorker
  :: MonadIO m
  => WorkerUnion -> WorkerName
  -> (SetWaiting -> SetBusy -> Worker -> IO (EventHandlerControl a))
  -> m Worker
newWorker wu handl f = liftIO $ do
  stat <- newMVar WorkerInit
  let myself tid = Worker
        { workerThreadId = tid
        , workerName     = handl
        , workerStatus   = stat
        }
  let waiting    = SetWaiting $ void $ swapMVar stat WorkerWaiting
  let busy       = SetBusy    $ void $ swapMVar stat WorkerBusy
  let workerLoop self = f waiting busy self >>= \ case
        EventHandlerContinue{} -> yield >> workerLoop self
        EventHandlerCancel     -> yield >> workerLoop self
        EventHandlerHalt       -> void $ swapMVar stat WorkerHalted
        EventHandlerFail   msg -> void $ swapMVar stat $ WorkerFailed msg
  fmap myself $ forkIO $ catches
    (do self <- myself <$> myThreadId
        bracket_ (unionize wu self) (retire wu self) (workerLoop self)
    )
    [ Handler $ \ (WorkerSignal ()) -> void $ swapMVar stat WorkerFlagged
    , Handler $ \ (SomeException e) -> do
        void (swapMVar stat $ WorkerFailed $ Strict.pack $ show e)
        throw e
    ]

-- | Signal a worker that they no longer need to perform their task. In this idylic little world, a
-- worker happily leaves when there is no more work to do, and they abandon their 'Worker' identity,
-- which is their membership in a 'WorkerUnion'. Calling 'relieveWorker' on the same 'Worker' ID
-- that is already retired does nothing.
relieveWorker :: MonadIO m => Worker -> m ()
relieveWorker (Worker{workerThreadId=tid}) = liftIO $ throwTo tid $ WorkerSignal ()

-- | Relieve members in a given 'WorkerUnion' which match a given predicate.
relieveWorkersIn :: MonadIO m => WorkerUnion -> (Worker -> Bool) -> m ()
relieveWorkersIn (WorkerUnion mvar) toBeRelieved = liftIO $ modifyMVar_ mvar $ \ set -> do
  let (toBe, notToBe) = Set.partition toBeRelieved set
  mapM_ relieveWorker $ Set.toList toBe
  return notToBe

-- | Like 'relieveWorkersIn' but the signals are sent to the workers default "government"
-- 'WorkerUnion'.
relieveGovWorkers :: (Worker -> Bool) -> GUI window model ()
relieveGovWorkers toBeRelieved = govWorkerUnion >>= liftIO . flip relieveWorkersIn toBeRelieved

-- | Define a new 'WorkerUnion'. 
newWorkerUnion :: MonadIO m => m WorkerUnion
newWorkerUnion = liftIO $ WorkerUnion <$> newMVar Set.empty

-- not for export
unionize :: WorkerUnion -> Worker -> IO ()
unionize (WorkerUnion mvar) worker = liftIO $
  modifyMVar_ mvar $ return . Set.insert worker

-- not for export
retire :: WorkerUnion -> Worker -> IO ()
retire (WorkerUnion mvar) worker = liftIO $
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
  writeChan (workPostInbox post) work
  postStats post $ _workPostWaitingInput %~ (+ 1)

-- | Check if @work@ being processed in a 'WorkPost' has completed. This function is
-- asynchronous. If there is no @work@ completed yet, 'Prelude.Nothing' is returned.
checkWork :: MonadIO m => WorkPost inWork outWork -> m (Maybe outWork)
checkWork post = liftIO $ do
  empty <- isEmptyChan $ workPostOutbox post
  if empty then return Nothing else
    Just <$> readChan (workPostOutbox post) <*
    postStats post (_workPostWaitingOutput %~ subtract 1)

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

-- $InstallingEventHandlers
--
-- These are functions your 'Happlet' can use to subscribe to receive certain types of events that
-- the back-end 'Happlets.Provider.Provider' may provide. Not all back-end
-- 'Happlets.Provider.Provider's provide all types of events. If the @window@ type you are using
-- does instantiate the below classes, you may program your 'Happlet' to receive that type of event.

-- | Windows that are manageable are part of multi-window GUI operating systems where a window
-- manager can display multiple windows, sending a signal when the window is made visible or
-- invisible, and sending a signal when the window received focus (is made to receive user input) or
-- loses focus.
class Managed window where
  -- | This event handler is evaluated when the window becomes visible or invisible
  visibleEvents :: (Bool -> GUI window model ()) -> GUI window model ()
  -- | This event handler is evaluated when the window gains or loses focus.
  focusEvents :: (Bool -> GUI window model ()) -> GUI window model ()
  -- | This function asks the operating system or desktop environment to show or hide the window
  -- associated with the given @window@. This should trigger a call to the event handler set by
  -- 'visibleEvents'.
  windowVisible :: Bool -> GUI window model ()

-- | This class must be instantiated by the 'Happlet.Provider.Provider' to evaluate 'GUI' functions
-- in 'Worker' threads. The 'guiWorker' makes use of this interface.
class CanRecruitWorkers window where
  -- | This function must assume that the given 'GUI' function will be evaluated in a new thread
  -- with the given 'GUIState'. This function must perform all locking of mutex variables necessary
  -- to evaluate the 'GUI' function, and update all mutexes with the results of evaluation.
  inOtherThreadRunGUI :: GUIState window model -> GUI window model a -> IO (EventHandlerControl a)

-- | Windows that can be resized can provide an instance of this class. Even on platforms where
-- there is only one window per screen, it may be possible to change screen sizes if the screen
-- resolutions or screen orientations can change.
class CanResize window where
  -- | This event handler is evaluated when the window is resized. It should NOT be called when the
  -- window is first initialized, or when it is first made visible. The 'NewPixSize' event value
  -- passed to the event handler 'GUI' function you provie to this event handler will always be
  -- exactly the same value as what is returned by 'getWindowSize'.
  resizeEvents
    :: CanvasMode
    -> (OldPixSize -> NewPixSize -> GUI window model ())
    -> GUI window model ()

-- | The event handler function set with 'resizeEvents' takes this parameter, which is the value of
-- the previous canvas size.
type OldPixSize = PixSize

-- | The event handler function set with 'resizeEvents' takes this parameter, which is the value of
-- the new canvas size.
type NewPixSize = PixSize

-- | When defining the behavior of your app for responding to window resize events (using
-- 'resizeEvents'), you have a choice of whether or not your app copies the old image to the new
-- canvas, or if you would prefer to just redraw everything from scratch. If you need to redraw
-- everything on a resize, this is common when objects in the image need to move around a lot when a
-- resize occurs, like in a text editor. If you are just going to redraw everything, then it would
-- be a pretty significant waste of CPU resources to copy the old canvas to the new canvas buffer
-- just before you drew over it. In this case, you would set your redraw beahvior to
-- 'ClearCanvasMode'.
--
-- However, sometimes you don't need to redraw everything, maybe you want the same exact image as
-- before, just with a new cropping frame, and if the window is larger than before you'll only
-- redraw the part of the buffer that was recently exposed. This can be done easily by setting
-- 'CopyCanvasMode', the Happlet back-end provider will automatically copy the old canvas buffer to
-- the new one after a resize event and before it calls your event handler.
--
-- You also have the option of using 'ScaleCanvasMode', which is like 'CopyCanvasMode' but scales
-- the image to the new buffer size. 'ScaleWidthCanvasMode' scales the image proportionally, meaning
-- a square will always be a square even when you resize the window to a rectanglular shape, and the
-- scaling factor is computed from the difference between the old width and the new
-- width. 'ScaleHeightCanvasMode' is similar but sets the scaling factor based on the new height.
data CanvasMode
  = ClearCanvasMode -- ^ don't bother copying the old canvas to the new one, we will just
  | CopyCanvasMode -- ^ copy the old canvas to the new before running the resize event handler.
  | ScaleCanvasMode -- ^ copy the old canvas and scale it to fill the new canvas.
  | ScaleWidthCanvasMode -- ^ copy the old canvas proprtionally from the new canvas width.
  | ScaleHeightCanvasMode -- ^ copy the old canvas proportionally from the new canvas height.
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | This class provides the ability to install animation event handlers which are repeatedly called
-- at fast, regular time intervals, usually 60 frames per second.
class CanAnimate window where
  -- | This function will be called repeatedly with a time delta indicating how much time has passed
  -- since the animation handler was installed. The given 'GUI' function should update the
  -- 'Happlet.View.Readraw' state each time.
  stepFrameEvents :: (AnimationMoment -> GUI window model ()) -> GUI window model ()
  -- | This function returns true of false if there is currently an animation event handler
  -- installed and running in the Happlet. To disable a running animation, evaluate
  -- @'stepFrameEvents' 'disable'@.
  animationIsRunning :: GUI window model Bool

-- | This class provides the ability to install keyboard event handlers.
class CanKeyboard window where
  -- | There may be more than one keyboard, although it is not likely. Just the same, it is possible
  -- to specify an 'Happlets.Event.InputDeviceId'. Remember that passing an empty list of
  -- 'Happlets.Event.InputDeviceId's will install the 'GUI' function to receive events from the
  -- default device.
  keyboardEvents :: (Keyboard -> GUI window model ()) -> GUI window model ()
  -- | This function should return a list of all possible keyboard devices. If an empty list is
  -- returned, only one device, the default device, is provided.
  providedKeyboardDevices :: GUI window model [InputDeviceId]
  providedKeyboardDevices = return []

-- | Mouse events can cause a lot of event throughput. To reduce this throughput and improve
-- efficiency, the 'mouseEvents' function take a parameter that lets you filter only certain mouse
-- events, so your 'GUI' function is only called when mouse events matching one of these patterns
-- occur.
data MouseEventPattern
  = MouseButton -- ^ Matches only mouse button click events.
  | MouseDrag   -- ^ Matches mouse motion events only when a button is clicked down.
  | MouseAll    -- ^ Matches all mouse motion and mouse button events.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | This class provides the ability to install mouse event handlers. Minimal complete definition is
-- 'mouseEvents'.
class CanMouse window where
  -- | Install a 'GUI' function that only responds to mouse button clicks. All mouse cursor motion
  -- is ignored.
  mouseEvents
    :: MouseEventPattern
    -> (Mouse -> GUI window model ())
    -> GUI window model ()
  -- | This function should return a list of all possible mouse devices. If an empty list is
  -- returned, only one device, the default device, is provided.
  providedMouseDevices :: GUI window model [InputDeviceId]
  providedMouseDevices = return []

-- | This class provides the ability to provide trackpad events handlers. Happlet programmers should
-- be not expect that many back-end providers will not provide events of this type at all.
--
-- 'Happlet.Event.Trackpad' events are a catch-all event type which cover all input devices that are
-- not keyboard or mouse devices. Back-end providers should strive to, as often as possible, eschew
-- a 'CanTrackpad' provider and define a 'CanMouse' provider a specific input device instead.
--
-- However, raw mouse events, stylus events, and touch screen events may all fall under the category
-- of 'CanTrackpad', so if the back-end providers would like to provide this type of event, it may.
class CanTrackpad window where
  -- | For 'Happlets.Event.Trackpad' events, it is important (but still not required) for you to
  -- specify an 'Happlets.Event.InputDeviceId'.
  trackpadEvents :: (Trackpad -> GUI window model ()) -> GUI window model ()
  -- | This function should return a list of all possible trackpad devices. If an empty list is
  -- returned, only one device, the default device, is provided. It is more important that back-end
  -- providers specify elements of this list, because of the wide variety of input devices that this
  -- event handler could be made to handle.
  providedTrackpadDevices :: GUI window model [InputDeviceId]
  providedTrackpadDevices = return []

----------------------------------------------------------------------------------------------------

-- | The back-end provider may provide it's own abstraction for an image buffer that satisfies the
-- functions of this type class.
class CanBufferImages window image render | window -> image, window -> render where
  -- | Create a new image buffer.
  newImageBuffer :: forall model a . PixSize -> render a -> GUI window model (a, image)
  -- | Resize the image buffer.
  resizeImageBuffer :: forall model a . image -> PixSize -> render a -> GUI window model a
  -- | Draw to the image buffer using a @render@ function.
  drawImage :: forall model a . image -> render a -> GUI window model a
  -- | Blit an image buffer to the current @window@. You can offset the location of the blit
  -- operation by passing an offset value as the 'Happlets.Readraw.PixCoord'.
  blitImage :: image -> PixCoord -> GUI window model ()
  -- | Like 'blitImage' but blits to another @image@, rather than to the current @window@. The first
  -- @image@ parameter is the source from which to blit, the second @image@ is the target to which
  -- to blit, followed by the offset and mask.
  blitImageTo
    :: Source image
    -> Target image
    -> Target PixCoord
    -> GUI window model ()

----------------------------------------------------------------------------------------------------

-- $OtherCapabilities
--
-- These are functions which can be called when defining a GUI function which has nothing to do with
-- reacting to events.

-- | Some back-end providers may wish to provide force-feedback functionality along with
-- 'CanTrackapd' event handlers.
class CanForceFeedback window where
  -- | Happlet programmers should be able to program a force feedback pattern by specifying a list
  -- of intensities and durations. Multiple calls to this function __should__ delete later force
  -- feedback patterns, and only execute the first call to this function. In multi-threaded systems,
  -- it is up to the back-end provider to decide what should happen if multiple threads call
  -- 'forceFeedback' at the same time. It is recommended that the force feedback sent to the input
  -- device should work on a first-come-first-serve basis, and later patterns should cancle current
  -- patterns that are currently being sent to the input device.
  forceFeedback :: [(FFIntensity, FFDuration)] -> GUI window model ()

-- | For back-end 'Happlets.Provider.Provider's that allow 'Happlet.GUI.Happlet's to subscribe to
-- events from specific devices, this function asks the back-end 'Happlets.Provider.Provider' to
-- return a list of valid input device identifiers that can be used when installing event handlers.
class DeviceHandler window where
  -- | Provide an up-to-date list of available 'Happlets.Event.InputDeviceId's.
  providedInputDevices :: GUI window model [InputDeviceId]
  -- | Subscriber functions which install 'GUI' event handlers, for example 'keyboardEvents',
  -- 'mouseEvents', 'trackpadEvents', or 'forceFeedback', will by default subscribe to events from
  -- any and all devices sending input from the back-end 'Happlets.Provider.Provider'. However if
  -- you wish for your 'Happlet' to only subscribe to events coming from only certain specific
  -- devices, evaluate these subscriber functions within this 'onDevices' function to limit which
  -- devices will send input to your 'GUI' functions.
  onDevices :: [InputDeviceId] -> GUI window model () -> GUI window model ()

-- | Returns a list of input devices that are not recognized.
unrecognizedInputDevices
  :: DeviceHandler window
  => [InputDeviceId] -> GUI window model [InputDeviceId]
unrecognizedInputDevices test = do
  have <- providedInputDevices
  return [device | device <- test, not $ device `elem` have]

-- | Similar to 'unrecognizedInputDevices' but will throw a runtime exception if any of the given
-- 'InputDeviceId's are not provided by the 'providedInputDevices'.
failUnrecognizedInputDevices
  :: DeviceHandler window
  => [InputDeviceId] -> GUI window model ()
failUnrecognizedInputDevices = unrecognizedInputDevices >=> \ bad ->
  if null bad then return () else fail $ "Unrecognized input device IDs"++show bad
