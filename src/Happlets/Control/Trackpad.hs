module Happlets.Control.Trackpad where

import           Happlets.Model.GUI
import           Happlets.Control.InputDevice

import           Data.Time.Clock

----------------------------------------------------------------------------------------------------

-- | Force feedback intensity, for systems which provided this functionality.
type FFIntensity = Double

-- | Force feedback duration in seconds.
type FFDuration = NominalDiffTime

-- | Not all Happlet back-end providers will make use of this event type.
--
-- This could be a track-pad or a joy-stick/joy-pad continuous signal, it usually encodes pinches,
-- swipe velocities, and/or rotations.
--
-- Really, this type is a catch-all for various other input devices that might be provided by a
-- Happlets back-end. On 64-bit systems, this data point contains 6 full 64-bit (a total of 48 byte)
-- values all passed at once, along with a device ID so it is obviously not the most efficient way
-- of doing reactive programming.
--
-- It is expceted that lots and lots of events of this type will be generated if it should be
-- provided and installed by a Happlet front-end programmer, and it is expected that perhaps many of
-- the events generated will be thrown away immediately. The 'InputDeviceId' type is provided to
-- mitigate this problem, so when using the 'Happlets.GUI.trackPadEvents' function, you can select
-- which devices from which you want to receive events and reduce the total number of events your
-- Happlet receives.
--
-- That said, it is still best for back-end providers to provide a 'Mouse' event handler instead,
-- and for front-end Happlet developers to use all 'Mouse' event handlers available to you.
data Trackpad
  = Trackpad
    { padDeviceID :: !InputDeviceId
      -- ^ This is a logical identifier for a trackpad device provided by the back-end. Names may
      -- include "raw-trackpad", "player1", "player2", "raw-mouse", "stylus", or "touchscreen". It
      -- is entirely platform/back-end dependent what logical 'TrackpadDeviceID's are available, so
      -- refer to the back-end provider documentation for how to use this.
    , padPinch    :: !Double -- ^ Negative value for pinch-in, positive value for pinch-out.
    , padTwist    :: !Double -- ^ Twisting angle delta in radians.
    , padPressure :: !Double
      -- ^ How much force is an end-user applying to the device.
    , padJoystickForwardLean  :: !Double
      -- ^ forward-lean angle in radians (accellerometer or joystick value)
    , padJoystickSidewaysLean :: !Double
      -- ^ left-right lean angle in radians (accellerometer or joystick value)
    }
  deriving (Eq, Show, Read)

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


