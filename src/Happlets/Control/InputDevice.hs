module Happlets.Control.InputDevice where

import qualified Data.Text as Strict

-- | An abstraction for input devices that are neither keyboards nor mouses. This goes for game
-- pads, joysticks, graphics tablets, touch screens, and motion-capture devices.
type InputDeviceId = Strict.Text
