-- | The Happlets library can be used with a few different back-end GUI libraries. This module
-- intends to provide an absraction over the most common similarities between all of these GUI
-- libraries. This allows developers using the Happlets library to rely mostly on the symbols
-- provided in this module to program Happlets in a platform-independent way.
module Happlets.Event where

import           Happlets.Draw.SampCoord

import           Control.Monad

import           Data.Bits
import qualified Data.ByteString as Strict
import           Data.Time.Clock
import           Data.Typeable
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | Animations are ultimately a sequence of discrete events, so time is tracked by a 'Prelude.Int'
-- value. Internally to your document @model@, you can keep track of time using a 'Prelude.Double'
-- or 'Data.Time.Clock.UTCTime', but you must instantiate the @model@ into the
-- 'Controller.Wire.Session.HasTime' class such that it converts this time value to an
-- 'Prelude.Int'.
type AnimationMoment = NominalDiffTime

----------------------------------------------------------------------------------------------------

-- | A wrapper type to isolate values that indicate some distance from the left of some window or
-- view screen.
newtype FromLeft num = FromLeft { unwrapFfromLeft :: num }
  deriving (Eq, Ord, Show, Read, Typeable, Num, Enum, Bounded)

-- | A wrapper type to isolate values that indicate some distance from the top of some window or
-- view screen.
newtype FromTop num = FromTop { unwrapFromTop :: num }
  deriving (Eq, Ord, Show, Read, Typeable, Num, Enum, Bounded)

-- | A wrapper type to isolate values that indicate some left-to-right width value of objects
-- visible in a window or on a view screen.
newtype Width num = Width { unwrapWidth :: num }
  deriving (Eq, Ord, Show, Read, Typeable, Num, Enum, Bounded)

-- | A wrapper type to isolate values that indicate some top-to-bottom width value of objects
-- visible in a window or on a view screen.
newtype Height num = Height { unwrapHeight :: num }
  deriving (Eq, Ord, Show, Read, Typeable, Num, Enum, Bounded)

----------------------------------------------------------------------------------------------------

-- | Used to encode keyboard event types in a platform independent way.
data Keyboard
  = Keyboard !Pressed !ModifierBits !KeyPoint
    -- ^ Happlet back-ends should try to encode keyboard events using this constructor.
  | RawKey   !Pressed !ModifierBits !Word32
    -- ^ For back-ends that cannot encode keyboard events using the above, use this as a fall-back.
  deriving (Eq, Show, Typeable)

-- | True if the keyboard or mouse event was a key/button-down or "pressed" event, False if the event was
-- key/button-up or "released" event.
type Pressed = Bool

-- | Keyboard event modifier bits. This is just an abstract, opaque, intermediate type used to
-- encode information from various Happlet back-ends.
data ModifierBits = ModifierBits Word32
  deriving (Eq, Ord, Bounded, Typeable)

instance Show ModifierBits where
  show = show . unpackModifiers

-- | Keyboard event information. This is just an abstract intermediate type used to encode
-- information from various Happlet back-ends in a platform-independent way. It has a small list of
-- symbols found on typical commercial keyboards.
data KeyPoint
  = CharKey !Char
  | FuncKey !Int
    -- ^ Arbitrary function keys. Typical keybords typically have keys labeled "F1" through "F12".
  | UpArrowKey
  | DownArrowKey
  | LeftArrowKey
  | RighArrowKey
  | TabKey
  | EnterKey
  | EscapeKey
  | BackSpaceKey
  | DeleteKey
  | HomeKey
  | EndKey
  | PageUpKey
  | PageDownKey
  | InsertKey
  | PuaseKey
  | SymbolKey Strict.ByteString
    -- ^ For keys not included in this list. This will typically be the logical name mapped to a raw
    -- keyboard point by the operating system.
  deriving (Eq, Show, Typeable)

-- | Typical modifier keys provided by most GUI back-ends, platform-independent. These values are
-- extracted from the 'ModifierBits' bit field 
data ModifierTag
  = Shift
  | CapsLock
  | LeftShift
  | RightShift
  | Ctrl
  | LeftCtrl
  | RightCtrl
  | Alt1
  | LeftAlt1
  | RightAlt1
  | Alt2
  | LeftAlt2
  | RightAlt2
  | Super1
  | LeftSuper1
  | RightSuper1
  | Super2
  | LeftSuper2
  | RightSuper2
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Typeable)

noModifiers :: ModifierBits
noModifiers = ModifierBits 0

packModifiers :: [ModifierTag] -> ModifierBits
packModifiers = ModifierBits . (foldl (.|.) 0) . fmap (shift 1 . fromEnum)

unpackModifiers :: ModifierBits -> [ModifierTag]
unpackModifiers (ModifierBits bits) = [minBound .. maxBound] >>= \ tag ->
  guard (shift 1 (fromEnum tag) .&. bits /= 0) >> [tag]

isAlt1 :: ModifierTag -> Bool
isAlt1 = \ case
  Alt1      -> True
  LeftAlt1  -> True
  RightAlt1 -> True
  _         -> False

isAlt2 :: ModifierTag -> Bool
isAlt2 = \ case
  Alt2      -> True
  LeftAlt2  -> True
  RightAlt2 -> True
  _         -> False

isAlt :: ModifierTag -> Bool
isAlt tag = isAlt1 tag || isAlt2 tag

isCtrl :: ModifierTag -> Bool
isCtrl = \ case
  Ctrl      -> True
  LeftCtrl  -> True
  RightCtrl -> True
  _         -> False

isShift :: ModifierTag -> Bool
isShift = \ case
  Shift      -> True
  LeftShift  -> True
  RightShift -> True
  _          -> False

isSuper1 :: ModifierTag -> Bool
isSuper1 = \ case
  Super1      -> True
  LeftSuper1  -> True
  RightSuper1 -> True
  _           -> False

isSuper2 :: ModifierTag -> Bool
isSuper2 = \ case
  Super2      -> True
  LeftSuper2  -> True
  RightSuper2 -> True
  _           -> False

isSuper :: ModifierTag -> Bool
isSuper tag = isSuper1 tag || isSuper2 tag

----------------------------------------------------------------------------------------------------

type InputDeviceId = Strict.ByteString

-- | An abstraction for mouse events. Includes whether a button was pressed or released, which
-- keyboard keys were pressed when the button was pressed, which buttons were pressed. An
-- 'InputDeviceId' is also included in the event information in the case that there are multiple
-- mouse devices provided by the back-end, especially for two-player games.
data Mouse
  = Mouse !InputDeviceId !Pressed !ModifierBits !MouseButton !PixCoord
  deriving (Eq, Ord, Show, Typeable)

data MouseButton
  = MotionOnly  -- ^ indicates no button is pressed but there is still a mouse motion event
  | LeftClick   -- ^ this could also map to a track-pad tap event
  | RightClick  -- ^ this could also map to a track-pad hold event
  | MiddleClick -- ^ this could also map to a track-pad double-tap event
  | SideClick   -- ^ some mouses have four buttons
  | VWheelClick -- ^ the vertical scroller wheel was clicked
  | HWheelClick -- ^ the horizontal scroller wheel was clicked
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

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
  deriving (Eq, Show, Read, Typeable)
