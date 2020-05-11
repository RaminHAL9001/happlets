module Happlets.Control.Mouse where

import           Happlets.Control.InputDevice
import           Happlets.Control.Keyboard
import           Happlets.Model.GUI
import           Happlets.View.SampCoord

----------------------------------------------------------------------------------------------------

-- | An abstraction for mouse events. Includes whether a button was pressed or released, which
-- keyboard keys were pressed when the button was pressed, which buttons were pressed. An
-- 'InputDeviceId' is also included in the event information in the case that there are multiple
-- mouse devices provided by the back-end, especially for two-player games.
data Mouse
  = Mouse !InputDeviceId !Pressed !ModifierBits !MouseButton !PixCoord
  deriving (Eq, Ord, Show)

data MouseButton
  = MotionOnly  -- ^ indicates no button is pressed but there is still a mouse motion event
  | LeftClick   -- ^ this could also map to a track-pad tap event
  | RightClick  -- ^ this could also map to a track-pad hold event
  | MiddleClick -- ^ this could also map to a track-pad double-tap event
  | SideClick   -- ^ some mouses have four buttons
  | VWheelClick -- ^ the vertical scroller wheel was clicked
  | HWheelClick -- ^ the horizontal scroller wheel was clicked
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

----------------------------------------------------------------------------------------------------

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
