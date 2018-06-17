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
    Happlet, HappletWindow(..), Display(..),

    -- * The GUI Function Type
    GUI, disable, failGUI, getModel, getSubModel, putModel, modifyModel,

    -- * Installing Event Handlers
    -- $InstallingEventHandlers
    Managed(..),
    CanResize(..),
    CanAnimate(..),
    CanMouse(..), MouseEventPattern(..),
    CanKeyboard(..),
    CanTrackpad(..),

    -- * Other Capabilities
    -- $OtherCapabilities
    CanBufferImages(..),
    CanForceFeedback(..),
    DeviceHandler(..), unrecognizedInputDevices, failUnrecognizedInputDevices,

    -- * Low-level details
    -- $LowLevel_Details
    GUIState(..), GUIContinue(..),
    guiModel, guiWindow, guiIsLive, evalGUI,
    makeHapplet, onHapplet, peekModel, sameHapplet,
    getGUIState, putGUIState, askHapplet,

  ) where

import           Prelude hiding ((.), id)

import           Happlets.Event
import           Happlets.Draw.SampCoord
import           Happlets.Draw.Types2D

import           Control.Arrow
import           Control.Category
import           Control.Concurrent.MVar
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
data Happlet model = Happlet { happletId :: Int, happletMVar :: MVar model }
  deriving (Eq, Typeable)

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
  mvar <- newMVar mod
  happId <- modifyMVar happletIdGen $ return . (id &&& id) . (+ 1)
  return Happlet{ happletId = happId, happletMVar = mvar }

-- | Lock the 'Happlet' and perform an IO function on the content of it.
onHapplet :: Happlet model -> (model -> IO (a, model)) -> IO (a, model)
onHapplet (Happlet{happletMVar=mvar}) f = modifyMVar mvar $
  liftM (\ (a, model) -> (model, (a, model))) . f

-- | Create a copy of the @model@ contained within the 'Happlet' from the current thread. Since this
-- @model@ is always being updated by various other threads, the value returned will only be a
-- snapshot of the @model@ at a particular point in time.
--
-- For the most part, a @model@ is only useful to the various 'Happlets.GUI.GUI' threads that run in
-- response to input events, but if it is necessary to view a snapshot of the @model@ from outside
-- of the event 'Happlet.GUI.GUI' proper, this is the function to use.
peekModel :: Happlet model -> IO model
peekModel (Happlet{happletMVar=mvar}) = readMVar mvar

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

  -- | Similar to 'doWindowNewHapplet', except places an existing 'Happlet' into the @window@,
  -- removing the previous 'Happlet'. This is effectivel a context switch that occurs within a
  -- single @window@. This function disables all event handlers, then evaluates the given 'GUI'
  -- function which should install new event handlers. This function then evaluates to 'disable', so
  -- any line of code in the @do@ block written after this function is called will never execute.
  windowChangeHapplet
    :: forall newmodel oldmodel . Happlet newmodel
    -> (PixSize -> GUI window newmodel ())
    -> GUI window oldmodel ()

  -- | Construct a 'GUI' function which evaluates a @render@ function that updates the buffer image
  -- of the @window@. Since this is the "canvas buffer" being updated, the images drawn by
  -- this function have much more permanence than images draw directly to the window with
  -- 'drawToWindow'. Images drawn with this function are copied back to the window whenever the
  -- window becomes "dirty," like when a your window is covered by another window from another
  -- application.
  onCanvas :: forall model a . (PixSize -> render a) -> GUI window model a

  -- | Similar to 'onCanvas' but does NOT draw to the buffer. By calling 'redrawRegion' you can
  -- "erase" portions of the view that were drawn by this function with the content of the buffer
  -- that was drawn by the 'onCanvas' function. Use this function only when you want to draw an image
  -- "temporarily," for example, when drawing an image that moves with the mouse cursor.
  onOSBuffer :: forall model a . (PixSize -> render a) -> GUI window model a

  -- | Force an area of the window to be redrawn by re-blitting the double buffer image to the
  -- window. Use this method to clear parts of the window that have been drawn over by the
  -- 'drawToWindow' function.
  refreshRegion :: [Rect2D SampCoord] -> GUI window model ()

  -- | Like 'refreshRegion' but refreshes the whole window. This function deletes everything drawn
  -- by the 'drawToWindow' function and replaces it with the content of the image drawn by 'onCanvas'.
  refreshWindow :: GUI window model ()

-- | This is a type class similar to the 'Prelude.Show' class, except it displays to the 'GUI'.
class Display drawable render | drawable -> render where
  display :: HappletWindow window render => drawable -> GUI window model ()

----------------------------------------------------------------------------------------------------

newtype GUI window model a
  = GUI
    { unwrapGUI ::
        ( ReaderT (Happlet model)
          ( ContT (GUIState window model) (StateT (GUIState window model) IO))
          a
        )
    }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | A data type indicating the condition of GUI evaluation. This value is part of the 'GUIState'
-- and is usually set when calling 'breakGUI'.
data GUIContinue
  = GUIHalt -- ^ Disable the event handler
  | GUIContinue -- ^ Allow the event handler to remain enabled for the next incoming event.
  | GUIFail !String -- ^ report a failure

-- | The 'GUIState' is the state that is constructed during the evaluation of a GUI function.
data GUIState window model
  = GUIState
    { theGUIModel    :: !model
    , theGUIWindow   :: !window
    , theGUIContinue :: !GUIContinue
    }

instance MonadState model (GUI window model) where
  state f = GUI $ lift $ lift $ state $ \ st ->
    let (a, model) = f (theGUIModel st) in (a, st{ theGUIModel = model })

instance MonadReader (Happlet model) (GUI window model) where
  ask = GUI ask
  local loc = GUI . local loc . unwrapGUI
  
instance Monad.MonadFail (GUI window model) where
  fail = breakGUI . (guiContinue .~) . GUIFail

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

-- | 'Control.Lens.Lens' for manipulating the 'GUIState'. This will get or set a function that
-- constructs a continuation to be used after evaluation of the 'GUI' function halts. This function
-- is usually called internally by the back-end 'Happlets.Provider.Provider', so it is better to not
-- use this function at all.
guiContinue :: Lens' (GUIState window model) GUIContinue
guiContinue = lens theGUIContinue $ \ a b -> a{ theGUIContinue = b }

-- | Evaluate a 'GUI' function on a given 'Happlet'. The 'Happlet' is locked durnig evaluation. If
-- the 'GUI' function evaluates to 'Disable', the given halt function (of type @IO ()@) is evaluated
-- before unlocking the 'Happlet', and then 'Prelude.False' is returned. If the 'GUI' function
-- evalautes to anything other than the 'Disable' value, the halt function is not evaluated and
-- 'Prelude.True' is returned.
evalGUI
  :: GUI window model void -- ^ the 'GUI' function to evaluate
  -> Happlet model
  -> window -> model
  -> IO (GUIState window model)
evalGUI (GUI f) happ win model =
  evalStateT (runContT (runReaderT (f >> lift (lift get)) happ) return) GUIState
    { theGUIModel    = model
    , theGUIWindow   = win
    , theGUIContinue = GUIContinue
    }

-- | Once you install an event handler, the default behavior is to leave the event handler installed
-- after it has evaluated so that it can continue reacting to events without you needing to re-install the event handler after each reaction.
--
-- However if you wish for an event handler to remove itself, evaluate this function as the final
-- function call in your 'GUI' procedure. Calling this function tells your 'GUI' function to
-- immediately halt and remove itself from the event handling loop.
--
-- Under the hood, this function will evaluate 'breakGUI' with a 'GUIHalt' condition set in the
-- 'guiContinue' field.
disable :: GUI window model void
disable = breakGUI $ guiContinue .~ GUIHalt

-- | Break out of the current 'GUI' evaluation context, performing a final updating function that
-- will be applied to the 'GUIState' before returning to @IO@. This function will never return, and
-- the evaluating context will immediately receive a result as soon as this function is evaluated.
breakGUI
  :: (GUIState window model -> GUIState window model)
  -> GUI window model void
breakGUI = GUI . ReaderT . const . ContT . const . (>> get) . modify

-- | Calling this function tells your 'GUI' function to immediately halt, reporting an error
-- condition. Under the hood, this will evaluate 'breakGUI' with a 'GUIFail' message set in the
-- 'guiContinue' field.
failGUI :: String -> GUI window model void
failGUI = breakGUI . (guiContinue .~) . GUIFail

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
guiIsLive :: GUIState window model -> Bool
guiIsLive = theGUIContinue >>> \ case { GUIContinue -> True; _ -> False; }

-- | Obtain a copy of the entire 'GUIState' data structure set for the current 'GUI' evaluation
-- context. This function should never be used unless you are programming a Happlet
-- 'Happlets.Provider.Provider'.
getGUIState :: GUI window model (GUIState window model)
getGUIState = GUI $ lift $ lift get

-- | Update the entire copy of the 'GUIState' data structure for the current context. This
-- function should never be used unless you are programming a Happlet 'Happlets.Provider.Provider'.
putGUIState :: GUIState window model -> GUI window model ()
putGUIState = GUI . lift . lift . put

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
  -- associated with the given @window@.
  windowVisible :: Bool -> GUI window model ()

-- | Windows that can be resized can provide an instance of this class. Even on platforms where
-- there is only one window per screen, it may be possible to change screen sizes if the screen
-- resolutions or screen orientations can change.
class CanResize window where
  -- | This event handler is evaluated when the window is resized. It should NOT be called when the
  -- window is first initialized, or when it is made visible
  resizeEvents :: (PixSize -> GUI window model ()) -> GUI window model ()

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

-- $OtherCapabilities
--
-- These are functions which can be called when defining a GUI function which has nothing to do with
-- reacting to events.

class CanBufferImages window image render | window -> image, window -> render where
  -- | Create a new image buffer.
  newImageBuffer :: forall model a . PixSize -> render a -> GUI window model image
  -- | Resize the image buffer.
  resizeImageBuffer :: forall model a . image -> PixSize -> render a -> GUI window model a
  -- | Draw to the image buffer using a @draw@ function.
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
