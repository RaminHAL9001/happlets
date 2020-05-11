module Happlets.Control.Animate where

import           Happlets.Model.GUI

import           Data.Time.Clock

----------------------------------------------------------------------------------------------------

-- | Animations are ultimately a sequence of discrete events, so time is tracked by a 'Prelude.Int'
-- value. Internally to your document @model@, you can keep track of time using a 'Prelude.Double'
-- or 'Data.Time.Clock.UTCTime', but you must instantiate the @model@ into the
-- 'Controller.Wire.Session.HasTime' class such that it converts this time value to an
-- 'Prelude.Int'.
type AnimationMoment = NominalDiffTime

-- | This class provides the ability to install animation event handlers which are repeatedly called
-- at fast, regular time intervals, usually 60 frames per second.
class CanAnimate provider where
  -- | This function will be called repeatedly with a time delta indicating how much time has passed
  -- since the animation handler was installed. The given 'GUI' function should update the
  -- 'Happlet.View.Readraw' state each time.
  stepFrameEvents :: (AnimationMoment -> GUI provider model ()) -> GUI provider model ()
  -- | This function returns true of false if there is currently an animation event handler
  -- installed and running in the Happlet. To disable a running animation, evaluate
  -- @'stepFrameEvents' 'disable'@.
  animationIsRunning :: GUI provider model Bool
