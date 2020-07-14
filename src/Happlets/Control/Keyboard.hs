module Happlets.Control.Keyboard where

import           Happlets.Model.GUI
import           Happlets.Control.InputDevice

import           Control.Monad                 (guard)

import           Data.Bits                     (Bits(..), (.|.))
import qualified Data.Text                     as Strict
import           Data.Word                     (Word8, Word32)

----------------------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------------

-- | Used to encode keyboard event types in a platform independent way.
data Keyboard
  = Keyboard !Pressed !ModifierBits !KeyPoint
    -- ^ Happlet back-ends should try to encode keyboard events using this constructor.
  | RawKey   !Pressed !ModifierBits !Word32
    -- ^ For back-ends that cannot encode keyboard events using the above, use this as a fall-back.
  deriving (Eq, Show)

-- | True if the keyboard or mouse event was a key/button-down or "pressed" event, False if the event was
-- key/button-up or "released" event.
type Pressed = Bool

-- | Keyboard event modifier bits. This is just an abstract, opaque, intermediate type used to
-- encode information from various Happlet back-ends. Instantiates 'Semigroup' with the
-- @('Data.Bits..|.')@ bitwise-OR function.
newtype ModifierBits = ModifierBits Word32
  deriving (Eq, Ord, Bounded, Bits)

instance Show ModifierBits where
  show = show . unpackModifiers

instance Semigroup ModifierBits where
  (<>) = (.|.)

instance Monoid ModifierBits where
  mempty = ModifierBits 0
  mappend = (<>)

-- | Keyboard event information. This is just an abstract intermediate type used to encode
-- information from various Happlet back-ends in a platform-independent way. It has a small list of
-- symbols found on typical commercial keyboards.
data KeyPoint
  = ModifierOnly
    -- ^ In some systems, a modifier key (like ShiftKey or Control) pressed alone will not generate an
    -- event until accompanied with a 'CharKey' or some other 'KeyPoint'. However sometimes a
    -- modifier key event is generated all by itself. When a modfier key is pressed alone, the
    -- 'KeyPoint' should be set to 'ModifierOnly'.
  | CharKey !Char
    -- ^ A letter key, which should be upper or lower case depending on whether the shift modifier
    -- is pressed or if the caps lock mode is enabled.
  | FuncKey !Word8
    -- ^ Arbitrary function keys. Typical keybords typically have keys labeled "F1" through "F12".
  | UpArrowKey
  | DownArrowKey
  | LeftArrowKey
  | RightArrowKey
  | TabKey
  | EnterKey
  | ReturnKey
  | EscapeKey
  | BackSpaceKey
  | DeleteKey
  | MenuKey
  | HomeKey
  | EndKey
  | PageUpKey
  | PageDownKey
  | InsertKey
  | PauseKey
  | BreakKey
  | SysRqKey
  | PrintScreenKey
  | ScrollLockKey
  | NumLockKey
  | KanjiKey
  | ZenkakuHankakuKey
  | MuhenkanKey
  | HenkanKey
  | HiraganaKatakanaKey
  | SymbolKey Strict.Text
    -- ^ For keys not included in this list. This will typically be the logical name mapped to a raw
    -- keyboard point by the operating system.
  deriving (Eq, Ord, Show)

-- | Typical modifier keys provided by most GUI back-ends, platform-independent. These values are
-- extracted from the 'ModifierBits' bit field 
data ModifierTag
  = ShiftKey
  | CapsLockKey
  | LeftShiftKey
  | RightShiftKey
  | CtrlKey
  | LeftCtrlKey
  | RightCtrlKey
  | AltKey
  | LeftAltKey
  | RightAltKey
  | CommandKey -- A second alt key, specific to Apple keyboards, also maps to NumLock on Linux
  | LeftCommandKey
  | RightCommandKey
  | Super1Key
  | LeftSuper1Key
  | RightSuper1Key
  | Super2Key
  | LeftSuper2Key
  | RightSuper2Key
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

noModifiers :: ModifierBits
noModifiers = mempty

packModifiers :: [ModifierTag] -> ModifierBits
packModifiers = ModifierBits . (foldl (.|.) 0) . fmap (shift 1 . fromEnum)

unpackModifiers :: ModifierBits -> [ModifierTag]
unpackModifiers (ModifierBits bits) = [minBound .. maxBound] >>= \ tag ->
  guard (shift 1 (fromEnum tag) .&. bits /= 0) >> [tag]

-- | True if any of 'AltKey', 'LeftAltKey', or 'RightAltKey' are set.
altIsSet :: ModifierBits -> Bool
altIsSet b = ModifierBits 0 /=
  b .&. packModifiers [AltKey, LeftAltKey, RightAltKey]

-- | True if any of 'CommandKey', 'RightCommandKey', or 'LeftCommandKey' are set.
commandIsSet :: ModifierBits -> Bool
commandIsSet b = ModifierBits 0 /=
  b .&. packModifiers [CommandKey, LeftCommandKey, RightCommandKey]

-- | True if any of 'Control', 'LeftControl', or 'RightControl' are set.
ctrlIsSet :: ModifierBits -> Bool
ctrlIsSet b = ModifierBits 0 /=
  b .&. packModifiers [CtrlKey, LeftCtrlKey, RightCtrlKey]

-- | True if any of 'ShiftKey', 'LeftShiftKey', or 'RightShiftKey' are set.
shiftIsSet :: ModifierBits -> Bool
shiftIsSet b = ModifierBits 0 /=
  b .&. packModifiers [ShiftKey, LeftShiftKey, RightShiftKey]

-- | True if any of 'Super1Key', 'LeftSuper1Key', or 'RightSuper1Key' are set.
super1IsSet :: ModifierBits -> Bool
super1IsSet b = ModifierBits 0 /=
  b .&. packModifiers [Super1Key, LeftSuper1Key, RightSuper1Key]

-- | True if any of 'Super1Key', 'LeftSuper1Key', or 'RightSuper1Key' are set.
super2IsSet :: ModifierBits -> Bool
super2IsSet b = ModifierBits 0 /=
  b .&. packModifiers [Super2Key, LeftSuper2Key, RightSuper2Key]

-- | True if any of 'Super1Key', 'Super2Key', 'LeftSuper1Key', 'LeftSuper2Key', 'RightSuper1Key', or
-- 'RightSuper2Key' have been set.
superIsSet :: ModifierBits -> Bool
superIsSet b = ModifierBits 0 /= b .&. packModifiers
  [Super1Key, Super2Key, LeftSuper1Key, LeftSuper2Key, RightSuper1Key, RightSuper2Key]

-- | True if the 'ModifierTag' is one of 'AltKey', 'LeftAltKey', or 'RightAltKey'
isAltKey :: ModifierTag -> Bool
isAltKey = \ case
  AltKey      -> True
  LeftAltKey  -> True
  RightAltKey -> True
  _        -> False

-- | True if the 'ModifierTag' is one of 'CommandKey', 'LeftCommandKey', or 'RightCommandKey'
isCommandKey :: ModifierTag -> Bool
isCommandKey = \ case
  CommandKey      -> True
  LeftCommandKey  -> True
  RightCommandKey -> True
  _            -> False

-- | True if the 'ModifierTag' is one of 'CtrlKey', 'LeftCtrlKey', or 'RightCtrlKey'
isCtrlKey :: ModifierTag -> Bool
isCtrlKey = \ case
  CtrlKey      -> True
  LeftCtrlKey  -> True
  RightCtrlKey -> True
  _         -> False

-- | True if the 'ModifierTag' is one of 'ShiftKey', 'LeftShiftKey', or 'RightShiftKey'
isShiftKey :: ModifierTag -> Bool
isShiftKey = \ case
  ShiftKey      -> True
  LeftShiftKey  -> True
  RightShiftKey -> True
  _          -> False

-- | True if the 'ModifierTag' is one of 'Super1Key', 'LeftSuper1Key', or 'RightSuper1Key'
isSuper1Key :: ModifierTag -> Bool
isSuper1Key = \ case
  Super1Key      -> True
  LeftSuper1Key  -> True
  RightSuper1Key -> True
  _           -> False

-- | True if the 'ModifierTag' is one of 'Super2Key', 'LeftSuper2Key', or 'RightSuper2Key'
isSuper2Key :: ModifierTag -> Bool
isSuper2Key = \ case
  Super2Key      -> True
  LeftSuper2Key  -> True
  RightSuper2Key -> True
  _           -> False

-- | True if either 'isSuper1Key' or 'isSuper2Key' are True.
isSuper :: ModifierTag -> Bool
isSuper tag = isSuper1Key tag || isSuper2Key tag
