-- | These are functions your 'Happlet' can use to subscribe to receive certain types of events that
-- the back-end 'Happlets.Provider.Provider' may provide. Not all back-end
-- 'Happlets.Provider.Provider's provide all types of events. If the @provider@ type you are using
-- does instantiate the below classes, you may program your 'Happlet' to receive that type of event.
module Happlets.Control.WindowManager where

import           Happlets.Model.GUI
import           Happlets.Provider.ConfigState
import           Happlets.View.Types2D

-- | Windows that are manageable are part of multi-window GUI operating systems where a window
-- manager can display multiple windows, sending a signal when the window is made visible or
-- invisible, and sending a signal when the window received focus (is made to receive user input) or
-- loses focus.
class Managed provider where

  -- | This event handler is evaluated when the window becomes visible or invisible
  visibleEvents :: (Bool -> GUI provider model ()) -> GUI provider model ()

  -- | This event handler is evaluated when the window gains or loses focus.
  focusEvents :: (Bool -> GUI provider model ()) -> GUI provider model ()

  -- | This function asks the operating system or desktop environment to show or hide the window
  -- associated with the given @provider@. This should trigger a call to the event handler set by
  -- 'visibleEvents'.
  windowVisible :: ConfigState (GUI provider model) Bool

  -- | Set the window size. The window manager may disallow this. The new window size is returned
  -- after resizing.
  windowSize :: ConfigState (GUI provider model) (Rect2D SampCoord)

  -- | Set or get whether the window is decorated (meaning, whether or not it has a border and title
  -- bar).
  windowDecorated :: ConfigState (GUI provider model) Bool
