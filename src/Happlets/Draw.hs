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
-- programmers to evaluate all of these functions using the 'Happlets.GUI.onCanvas' function and
-- perform the expected update on the Happlet window.
class (Functor render, Applicative render, Monad render, MonadIO render)
   => Happlet2DGraphics render where
  -- | Set all pixels in the screen graphics to the same color value, deleting all graphics that
  -- existed prior.
  clearScreen :: FillColor -> render ()

  -- | Draw a line on the canvas.
  drawLine :: RealFrac n => LineColor -> LineWidth n -> Line2D n -> render ()

  -- | Draw a sequence of lines from a list of points, from head-to-tail, on the canvas. Passing an
  -- empty list should result in a no-op. Passing a list with a single point should paint the point.
  drawPath :: RealFrac n => LineColor -> LineWidth n -> [Point2D n] -> render ()

  -- | Draw a rectangle on the canvas.
  drawRect :: RealFrac n => LineColor -> LineWidth n -> FillColor -> Rect2D n -> render ()

  -- | Update a pixel value somewhere on the canvas.
  setPoint :: RealFrac n => Point2D n -> Color -> render ()

  -- | Read a pixel value somewhere on the canvas. Out-of-bounds indicies throw an exception.
  getPoint :: RealFrac n => Point2D n -> render Color

modifyPoint
  :: (Monad render, Happlet2DGraphics render, RealFrac n)
  => Point2D n -> (Color -> Color) -> render ()
modifyPoint p f = getPoint p >>= setPoint p . f
