module Happlets.Control.Mouse where

import           Happlets.View.Types2D (SampCoord, PixCoord, V2(..))
import           Happlets.Control.InputDevice
import           Happlets.Control.Keyboard
import           Happlets.Model.GUI

----------------------------------------------------------------------------------------------------

-- | A low-level abstraction for mouse events. Values of this type are received in a stream from the
-- operating system, although typically this information is not polled as an ordinary stream signal
-- would be, any change to the mouse state generates a signal value of this data type which can
-- trigger 'GUI' event handlers.
--
-- Information stored in this data type include whether a button was pressed or released, which
-- keyboard keys were pressed when the button was pressed, which buttons were pressed. An
-- 'InputDeviceId' is also included in the event information in the case that there are multiple
-- mouse devices provided by the back-end, especially for two-player games.
--
-- This data type does not contain information about clicks, double-clicks, or drags, which is why
-- it is a "signal" and not an "event." For events, you need to use a 'Happlets.Scene.Act' and
-- 'Happlets.Scene.Scene' which has it's own high-level event handlers based on this low-level
-- 'MouseSignal' data type. Or you can use a functional reactive programming framework and establish
-- your own methods of translating signals to events.
data MouseSignal
  = MouseSignal !InputDeviceId !Pressed !ModifierBits !MouseButtonSignal !PixCoord
  deriving (Eq, Ord, Show)

-- | Which button was pressed. This data structure accomodates a large variety of typical mouse
-- types, but most applications will only ever be interested in 'LeftClick', 'RightClick,' or
-- 'MotionOnly'.
data MouseButtonSignal
  = MotionOnly  -- ^ indicates no button is pressed but there is still a mouse motion event
  | LeftClick   -- ^ this could also map to a track-pad tap event
  | RightClick  -- ^ this could also map to a track-pad hold event
  | MiddleClick -- ^ this could also map to a track-pad double-tap event
  | SideClick   -- ^ some mouses have four buttons
  | VWheelClick -- ^ the vertical scroller wheel was clicked
  | HWheelClick -- ^ the horizontal scroller wheel was clicked
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | To help with de-bouncing (on platforms where it is necessary) you can compare if two mouse
-- events are similar and possibly caused by an input device registering an event twice. A mouse
-- event is "similar" if it has the same pixel coordinate location, the same mouse button, and the
-- same modifier bits set. Only the "pressed" status is not considered when computing similarity, as
-- it is the pressed status that changes rapidly when a bounce occurs.
similarMouseSignals :: MouseSignal -> MouseSignal -> Bool
similarMouseSignals (MouseSignal _ _ modA butnA ptA) (MouseSignal _ _ modB butnB ptB) =
  modA == modB &&
  butnA == butnB &&
  ptA == ptB

-- | Compute the square of the distance (in pixels) between two 'Mouse' event data structures. This
-- allows you to decide whether two mouse events occurred close-enough together for the events to
-- have occurred at the same place.
mouseSignalDistance :: MouseSignal -> MouseSignal -> SampCoord
mouseSignalDistance (MouseSignal _ _ _ _ (V2 xA yA)) (MouseSignal _ _ _ _ (V2 xB yB)) =
  let dx = xA - xB in
  let dy = yA - yB in
  dx * dx + dy * dy

----------------------------------------------------------------------------------------------------

-- | Mouse events can cause a lot of event throughput. To reduce this throughput and improve
-- efficiency, the 'mouseEvents' function take a parameter that lets you filter only certain mouse
-- events, so your 'GUI' function is only called when mouse events matching one of these patterns
-- occur.
data MouseSignalPattern
  = MouseButton -- ^ Matches only mouse button click events.
  | MouseDrag   -- ^ Matches mouse motion events only when a button is clicked down.
  | MouseAll    -- ^ Matches all mouse motion and mouse button events.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | This class provides the ability to install mouse event handlers. Minimal complete definition is
-- 'mouseEvents'.
class CanMouse window where
  -- | Install a 'GUI' function that only responds to mouse button clicks. All mouse cursor motion
  -- is ignored.
  mouseSignals
    :: MouseSignalPattern
    -> (MouseSignal -> GUI window model ())
    -> GUI window model ()
  -- | This function should return a list of all possible mouse devices. If an empty list is
  -- returned, only one device, the default device, is provided.
  providedMouseDevices :: GUI window model [InputDeviceId]
  providedMouseDevices = return []
