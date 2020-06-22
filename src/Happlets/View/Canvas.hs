-- | This module defines abstractions for working with top-level windows provided by an operating
-- system with a GUI. It is a good idea for window providers instantiaing these typeclasses to also
-- intantiate the typeclasses in 'Happlet.Control.Resize' and 'Happlet.Control.WindowManager'.
module Happlets.View.Canvas where

import           Happlets.Model.GUI
import           Happlets.View.Types2D
import           Happlets.Control.Resize
import           Happlets.Provider.Variable

import           Control.Exception

import qualified Data.Text                as Strict

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
class HappletWindow provider render | provider -> render where
  -- | This event handler is called when 'windowChangeHapplet' is called, allowing one final event
  -- handler to be called for cleaning-up, before the current 'Happlet' is detached from the
  -- @provider@.
  changeEvents :: GUI provider oldmodel () -> GUI provider oldmodel ()

  -- | Set the clip region of the current window. The coordinates used by all drawing operations
  -- will be translated such that the upper-left corner of the 'Happlets.Draw.Types2D.Rect2D' within
  -- the window will be the point @(0, 0)@ from the perspective of the 'onCanvas' @render@ing
  -- function. Only the top element of the clip region stack is used, the remainder of the stack is
  -- ignored. Setting 'Nothing' clears the region so that drawing operations can be applied to the
  -- entire window.
  windowClipRegion :: Variable (GUI provider model) (Maybe (Rect2D SampCoord))

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
  onCanvas :: forall model a . render a -> GUI provider model a

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
  onOSBuffer :: forall model a . render a -> GUI provider model a

  -- | Force an area of the window to be redrawn by re-blitting the double buffer image to the
  -- window. Use this method to clear parts of the window that have been drawn over by the
  -- 'drawToWindow' function.
  refreshRegion :: [Rect2D SampCoord] -> GUI provider model ()

  -- | Like 'refreshRegion' but refreshes the whole window. This function deletes everything drawn
  -- by the 'drawToWindow' function and replaces it with the content of the image drawn by
  -- 'onCanvas'.
  refreshWindow :: GUI provider model ()

----------------------------------------------------------------------------------------------------

data CanvasIOError
  = CanvasIOFilePath IOException
  | CanvasIOFileType Strict.Text
  deriving (Eq, Show)

class MonadIO render
  => HappletPixelBuffer provider render | provider -> render where
  -- | Change the 'CanvasResizeMode' value associated with the @image@ value. Example of how to you might
  -- use this variable:
  --
  -- @
  -- img <- 'newImageBuffer' ('V2' 540 270) ('clearScreen' 'white')
  -- 'setVal' ('imageCanvasResizeMode' img) 'CanvasResizeClear'
  -- @
  imageCanvasResizeMode :: Variable (GUI provider model) CanvasResizeMode

  -- | Resize the image buffer.
  resizeImageBuffer :: PixSize -> render a -> GUI provider model a

-- | Similar to the typeclass 'HappletWindow', but provides additional functions for loading pixel
-- buffers from a file, and saving to a file.
class HappletPixelBufferIO provider render | provider -> render where
  loadPixelBuffer :: FilePath -> GUI provider model ()
  savePixelBuffer :: FilePath -> GUI provider model ()
