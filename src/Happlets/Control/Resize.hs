module Happlets.Control.Resize where

import           Happlets.Model.GUI
import           Happlets.View.SampCoord
import           Happlets.Provider.Variable

----------------------------------------------------------------------------------------------------

-- | The event handler function set with 'resizeEvents' takes this parameter, which is the value of
-- the previous canvas size.
type OldPixSize = PixSize

-- | The event handler function set with 'resizeEvents' takes this parameter, which is the value of
-- the new canvas size.
type NewPixSize = PixSize

-- | When defining the behavior of your app for responding to window resize events (using
-- 'resizeEvents'), you have a choice of whether or not your app copies the old image to the new
-- canvas, or if you would prefer to just redraw everything from scratch. If you need to redraw
-- everything on a resize, this is common when objects in the image need to move around a lot when a
-- resize occurs, like in a text editor. If you are just going to redraw everything, then it would
-- be a pretty significant waste of CPU resources to copy the old canvas to the new canvas buffer
-- just before you drew over it. In this case, you would set your redraw beahvior to
-- 'CanvasResizeClear'.
--
-- However, sometimes you don't need to redraw everything, maybe you want the same exact image as
-- before, just with a new cropping frame, and if the window is larger than before you'll only
-- redraw the part of the buffer that was recently exposed. This can be done easily by setting
-- 'CanvasResizeCopy', the Happlet back-end provider will automatically copy the old canvas buffer to
-- the new one after a resize event and before it calls your event handler.
--
-- You also have the option of using 'CanvasResizeScale', which is like 'CanvasResizeCopy' but
-- scales the image to the new buffer size.
--
-- For resize functions more complicated than what is provided these options, it is best to set
-- 'CanvasResizeClear' and perform the redraw yourself.
data CanvasResizeMode
  = CanvasResizeClear
    -- ^ Don't bother copying the old canvas to the new one, you can redraw the entire canvas again.
  | CanvasResizeCopy
    -- ^ Copy the old canvas to the new before running the resize event handler. This is useful in
    -- the situation in which you intend to expose more or less of a larger buffered image, and you
    -- only need to copy that image to the newly exposed area.
  | CanvasResizeScale
    -- ^ Copy the old canvas and scale it to fill the new canvas. This is useful when you want to
    -- preserve an image of the pixel buffer in the canvas as it is resized, but redraw it to the
    -- new scale after the resize operation ends.
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Windows that can be resized can provide an instance of this class. Even on platforms where
-- there is only one window per screen, it may be possible to change screen sizes if the screen
-- resolutions or screen orientations can change.
class CanResize provider where
  -- | This event handler is evaluated when the window is resized. It should NOT be called when the
  -- window is first initialized, or when it is first made visible. The 'NewPixSize' event value
  -- passed to the event handler 'GUI' function you provie to this event handler will always be
  -- exactly the same value as what is returned by 'getWindowSize'.
  resizeEvents
    :: CanvasResizeMode
    -> (OldPixSize -> NewPixSize -> GUI provider model ())
    -> GUI provider model ()

  windowResizeMode :: Variable (GUI provider model) CanvasResizeMode
