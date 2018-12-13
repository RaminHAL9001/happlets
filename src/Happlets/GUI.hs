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
    cancelIfBusy, howBusy, deleteEventHandler, bracketGUI,

    -- * Widgets
    Widget, widget, widgetState, widgetBoundingBox, onWidget, mouseOnWidget,
    widgetContainsPoint, widgetContainsMouse, theWidgetState,

    -- * Installing Event Handlers
    -- $InstallingEventHandlers
    Managed(..),
    CanResize(..), OldPixSize, NewPixSize, CanvasMode(..),
    CanAnimate(..),
    CanMouse(..), MouseEventPattern(..),
    CanKeyboard(..),
    CanTrackpad(..),

    -- * Buffered Images
    CanBufferImages(..),

    -- * Other Capabilities
    -- $OtherCapabilities
    CanForceFeedback(..),
    DeviceHandler(..), unrecognizedInputDevices, failUnrecognizedInputDevices,

    -- * Low-level details
    -- $LowLevel_Details
    GUIState(..), EventHandlerControl(..),
    guiModel, guiWindow, guiIsLive, execGUI, runGUI,
    makeHapplet, onHapplet, peekModel, sameHapplet,
    getGUIState, putGUIState, modifyGUIState, askHapplet,

  ) where

import           Prelude hiding ((.), id)

import           Happlets.Event
import           Happlets.Draw.SampCoord
import           Happlets.Draw.Types2D

import           Control.Arrow
import           Control.Category
import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.Cont
import qualified Control.Monad.Fail    as Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Semigroup
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
  -- @window@. The event handler receives a reference to the 'Happlet' that will be replacing it, so
  -- if the old 'Happlet' needs to communicate some important message to the new 'Happlet' using
  -- 'onHapplet' it can do so.
  changeEvents :: (Happlet newmodel -> GUI window oldmodel ()) -> GUI window oldmodel ()

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

newtype GUI window model a
  = GUI
    { unwrapGUI ::
        ReaderT (Happlet model) (StateT (GUIState window model) IO) (EventHandlerControl a)
    }
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
  | EventHandlerFail !String
    -- ^ report a failure
  deriving (Eq, Show, Functor)

-- | The 'GUIState' is the state that is constructed during the evaluation of a GUI function.
data GUIState window model
  = GUIState
    { theGUIModel   :: !model
    , theGUIHapplet :: !(Happlet model)
    , theGUIWindow  :: !window
    }

instance Applicative (GUI window model) where { pure = return; (<*>) = ap; }

instance Monad (GUI window model) where
  return = GUI . return . EventHandlerContinue
  (GUI f) >>= next = GUI $ ReaderT $ \ happlet -> StateT $ \ st ->
    runStateT (runReaderT f happlet) st >>= \ (a, st) -> case a of
      EventHandlerContinue a -> runStateT (runReaderT (unwrapGUI $ next a) happlet) st
      EventHandlerHalt       -> return (EventHandlerHalt    , st)
      EventHandlerCancel     -> return (EventHandlerCancel  , st)
      EventHandlerFail   err -> return (EventHandlerFail err, st)

instance MonadIO (GUI window model) where
  liftIO f = GUI $ liftIO $ EventHandlerContinue <$> f

instance MonadState model (GUI window model) where
  state f = GUI $ lift $ state $ \ st ->
    let (a, model) = f (theGUIModel st) in (EventHandlerContinue a, st{ theGUIModel = model })

instance MonadReader (Happlet model) (GUI window model) where
  ask = GUI $ EventHandlerContinue <$> ask
  local loc = GUI . local loc . unwrapGUI
  
instance Monad.MonadFail (GUI window model) where
  fail = GUI . return . EventHandlerFail

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
-- evaluate it.
runGUI
  :: GUI window model a -- ^ the 'GUI' function to evaluate
  -> Happlet model
  -> GUIState window model
  -> IO (EventHandlerControl a, GUIState window model)
runGUI (GUI f) happlet st = runStateT (runReaderT f happlet) st

execGUI
  :: GUI window model a -- ^ the 'GUI' function to evaluate
  -> Happlet model
  -> GUIState window model
  -> IO (GUIState window model)
execGUI f happlet = fmap snd . runGUI f happlet

-- | Like the 'Control.Exception.bracket' function, but works in the 'GUI' monad.
bracketGUI
  :: GUI window model open
  -> GUI window model closed
  -> GUI window model a
  -> GUI window model a
bracketGUI lock unlock f = GUI $ ReaderT $ \ happlet -> StateT $ \ st -> do
  mvar <- newEmptyMVar
  a    <- bracket
    (runGUI lock happlet st >>= \ (rsrc, st) -> putMVar mvar st >> return rsrc)
    (const $ modifyMVar mvar $ fmap (\ (a,b)->(b,a)) . runGUI unlock happlet)
    (\ case
      EventHandlerContinue{} -> modifyMVar mvar $ fmap (\ (a,b)->(b,a)) . runGUI f happlet
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
howBusy = GUI (lift $ EventHandlerContinue <$> gets theGUIHapplet) >>= liftIO . fmap fst . getWaitingThreads

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
getGUIState = GUI $ lift $ EventHandlerContinue <$> get

-- | Update the entire copy of the 'GUIState' data structure for the current context. This
-- function should never be used unless you are programming a Happlet 'Happlets.Provider.Provider'.
putGUIState :: GUIState window model -> GUI window model ()
putGUIState = GUI . lift . fmap EventHandlerContinue . put

-- | Modify the 'GUIState' data structure for the current context. This function should never be
-- used unless you are programming a Happlet 'Happlets.Provider.Provider'
modifyGUIState :: (GUIState window model -> GUIState window model) -> GUI window model ()
modifyGUIState = GUI . lift . fmap EventHandlerContinue . modify

-- | Obtain a reference to the 'Happlet' in which this 'GUI' function is being evaluated.
askHapplet :: GUI window model (Happlet model)
askHapplet = ask

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
