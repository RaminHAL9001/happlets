-- | This module re-exports the "Happlets.Draw.Color" and "Happlets.Draw.Types2D" modules, and also
-- provides the 'HappletDrawing' class, which is a minimalist set of drawing primitives functions
-- for updateing the graphics in a Happlet window.
--
-- Future work on the Happlets project may result in the creation of a new package, perhaps called
-- @happlets-draw-ext@, which provides extended drawing primitives for a practical 2D graphics
-- library that can export to and import from SVG data, however no plans for such an extension have
-- been made at the time of this writing.
module Happlets.Draw
  ( module Happlets.Draw.Color
  , module Happlets.Draw.SampCoord
  , module Happlets.Draw.Text
  , module Happlets.Draw.Types2D
  , Happlet2DGraphics(..), modifyPoint,
  ) where

import           Happlets.Draw.Color
import           Happlets.Draw.SampCoord
import           Happlets.Draw.Text
import           Happlets.Draw.Types2D

import           Control.Monad.IO.Class

----------------------------------------------------------------------------------------------------

-- | This is a minimalist set of of drawing primitives functions for updating the graphics in a
-- Happlet window. This class must instantiate 'Control.Monad.Monad' and
-- 'Control.Monad.IO.Class.MonadIO' because it is expected to perform stateful updates to the
-- system, namely changing what graphics are displayed in a Happlet window.
--
-- The @render@ data type instantiated here is expected to be the the same as the @render@ type
-- instantiated into the 'Happlets.GUI.HappletWindow' type class, which should allow Happlet
-- programmers to evaluate all of these functions using the 'Happlets.GUI.onView' function and
-- perform the expected update on the Happlet window.
class (Functor render, Applicative render, Monad render, MonadIO render)
   => Happlet2DGraphics render where
  -- | Set all pixels in the screen graphics to the same color value, deleting all graphics that
  -- existed prior.
  clearScreen :: FillColor -> render ()

  -- | Draw a line on the canvas.
  drawLine :: LineColor -> LineWidth -> Line2D RealApprox -> render ()

  -- | Draw a sequence of lines from a list of points, from head-to-tail, on the canvas. Passing an
  -- empty list should result in a no-op. Passing a list with a single point should paint the point.
  drawPath :: LineColor -> LineWidth -> [Point2D RealApprox] -> render ()

  -- | Draw a rectangle on the canvas.
  drawRect :: LineColor -> LineWidth -> FillColor -> Rect2D RealApprox -> render ()

  -- | Update a pixel value somewhere on the canvas.
  setPoint :: Point2D RealApprox -> PackedRGBA32 -> render ()

  -- | Read a pixel value somewhere on the canvas. Out-of-bounds indicies throw an exception.
  getPoint :: Point2D RealApprox -> render PackedRGBA32

modifyPoint
  :: (Monad render, Happlet2DGraphics render)
  => Point2D RealApprox -> (PackedRGBA32 -> PackedRGBA32) -> render ()
modifyPoint p f = getPoint p >>= setPoint p . f
