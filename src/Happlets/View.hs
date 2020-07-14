-- | This module re-exports the "Happlets.Draw.Color" and "Happlets.Draw.Types2D" modules, and also
-- provides the 'HappletDrawing' class, which is a minimalist set of drawing primitives functions
-- for updateing the graphics in a Happlet window.
--
-- Future work on the Happlets project may result in the creation of a new package, perhaps called
-- @happlets-draw-ext@, which provides extended drawing primitives for a practical 2D graphics
-- library that can export to and import from SVG data, however no plans for such an extension have
-- been made at the time of this writing.
module Happlets.View
  ( -- * 2D Graphics Typeclass
    Happlet2DGraphics(..), BlitOperator(..), modifyPixel, defaultClearScreen,
    -- * Image Buffers
    Happlet2DBuffersPixels(..), Source, Target,
    -- * Re-Exports
    module Happlets.View.Color,
    module Happlets.View.SampCoord,
    module Happlets.View.Text,
    module Happlets.View.Types2D,
    module Happlets.View.Canvas,
  ) where

import           Happlets.View.Canvas
import           Happlets.View.Color
import           Happlets.View.SampCoord
import           Happlets.View.Text
import           Happlets.View.Types2D
import           Happlets.Provider.ConfigState

import           Control.Lens ((&), (.~))
import           Control.Monad.IO.Class

----------------------------------------------------------------------------------------------------

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Source image = image

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Target image = image

-- | This is a set of of drawing primitives functions for updating the graphics in a Happlet window,
-- which is essentially a subset of Cairo Graphics or Rasterific. This class must instantiate
-- 'Control.Monad.Monad' and 'Control.Monad.IO.Class.MonadIO' because it is expected to perform
-- impure (side-effectful) operations, in this case, changing the contents of graphics memory.
--
-- This typeclass is fairly useless on its own, Happlet providers instantiating their @render@
-- function type into this class will also need to instantiate 'Happlet2DGeometry',
-- 'Happlet2DBuffersPixels', or both. Otherwise, the 'fill' and 'stroke' functions provided in this
-- typeclas here have little meaning.
class (Functor render, Applicative render, Monad render, MonadIO render)
  => Happlet2DGraphics render where
  -- | Lookup or update a pixel value at the given 'Point2D' position on the canvas immediately,
  -- without otherwise changing anything in the graphics context of the @render@ function. This
  -- function must not wait until 'fill' or 'stroke' is called to make the update.
  --
  -- Example of how you Happlet programmers would use this function:
  --
  -- @
  -- -- Set the pixel at point 30, 50 to 'Happlets.Draw.Color.red'
  -- 'pixel' ('Linear.V2.V2' 30 50) => Color 'red'
  --
  -- -- Get the color at point 45, 45
  -- @ 'getEnv' $ 'pixel' $ 'Linear.V2.V2 45 45
  pixel :: Point2D SampCoord -> ConfigState render Color

  -- | This function should save the current drawing context, then evaluate a continuation function
  -- of type @render@, then restore the current drawing context
  tempContext :: render a -> render a

  -- | This function should restore the default graphics context state, deleting any 'shape's or
  -- 'fillPattern's currently set.
  resetGraphicsContext :: render ()

  -- | Evaluate the drawing instructions given by the 'Draw2DPrimitive', with coordinates converted
  -- to canvas pixel-local coordinates, and an optional bounding box clip region.
  draw2D :: Maybe (Rect2D SampCoord) -> [Draw2DPrimitive SampCoord] -> render ()

  -- | Set all pixels in the screen graphics to the same color value given by the 'fillColor',
  -- deleting all graphics that existed prior. This function does not reset the 'clipRegion'.
  clearScreen :: Color -> render ()

-- | This function provides a default implementation for 'clearScreen' for the 'Happlet2DGraphics'
-- typeclass, as long as the @render@ function that instantiates the 'Happlet2DGraphics' typeclass
-- also instantiates the 'Happle2DHasDrawing' typeclass as well.
defaultClearScreen :: Happlet2DGraphics render => Color -> render ()
defaultClearScreen c = tempContext $ do
  resetGraphicsContext
  draw2D Nothing [prim2D & drawPrimFill .~ Just (paintColor c)]

-- | This class extends the 'Happlet2DGraphics' class with operators for copying sections of the
-- operand canvas into a pixel buffer.
class (Functor render, Applicative render, Monad render, MonadIO render)
  => Happlet2DBuffersPixels render pixbuf where
    -- | Create a copy of a portion of the canvas
    copyRegion :: Rect2D SampCoord -> render pixbuf

    -- | Create a new blank canvas
    newSubCanvas :: Size2D SampCoord -> render pixbuf

    -- | Evaluate a @render@ continuation function on a given @pixbuf@ canvas. When 'withSubCanvas'
    -- returns, drawing operations will continue to work on the main canvas.
    withSubCanvas :: pixbuf -> render a -> render a

-- | Calls @'getEnv' 'pixel'@, applies a function @f@, then calls @'setEnv' 'pixel'@ with the
-- result.
modifyPixel
  :: (Monad render, Happlet2DGraphics render)
  => Point2D SampCoord -> (Color -> Color) -> render ()
modifyPixel p f = getConfig (pixel p) >>= setConfig (pixel p) . f
