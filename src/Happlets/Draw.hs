-- | This module re-exports the "Happlets.Draw.Color" and "Happlets.Draw.Types2D" modules, and also
-- provides the 'HappletDrawing' class, which is a minimalist set of drawing primitives functions
-- for updateing the graphics in a Happlet window.
--
-- Future work on the Happlets project may result in the creation of a new package, perhaps called
-- @happlets-draw-ext@, which provides extended drawing primitives for a practical 2D graphics
-- library that can export to and import from SVG data, however no plans for such an extension have
-- been made at the time of this writing.
module Happlets.Draw
  ( -- * 2D Graphics Typeclass
    Happlet2DGraphics(..), BlitOperator(..), Happlet2DGeometry(..), Happlet2DBuffersPixels(..),
    modifyPixel,
    -- * Fill Types
    PaintSource(..), GradientType(..), GradientStopList, GradientStop(..),
    gradStopsFromList, gradStopsToList, gradStopPoint, gradStopColor,
    -- * Line Style
    HasLineStyle(..), LineStyle(..), theLineColour,
    makeLineStyle, lineColor, lineColour, lineWeight,
    -- * Re-Exports
    module Happlets.Draw.Color,
    module Happlets.Draw.SampCoord,
    module Happlets.Draw.Text,
    module Happlets.Draw.Types2D
  ) where

import           Happlets.Draw.Color
import           Happlets.Draw.SampCoord
import           Happlets.Draw.Text
import           Happlets.Draw.Types2D
import           Happlets.Variable

import           Control.Lens (Lens', lens, (%~))
import           Control.Monad.IO.Class

import qualified Data.Vector.Unboxed as UVec
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | This sets the blitting function, that is, when an object is blitted to the canvas, how are the
-- pixels that are already on the canvas (the destination) combined with the new pixels (the
-- source).
--
-- These operators are a subset of the Cairo Graphics operators:
--
-- <<https://www.cairographics.org/operators/>>
data BlitOperator
  = BlitSource
    -- ^ Overwrite the canvas pixel with the blit pixel using the 'const' function, same as the
    -- @source@ operator in Cairo Graphics.
  | BlitOver
    -- ^ Takes a weighted average, where the weight of the source pixel is it's alpha value @a@, the
    -- weight of the destination pixel is @(1-a)@ (one minus the blit pixel weight). This is the
    -- same as the @over@ operator in Cairo Graphics.
  | BlitXOR
  | BlitAdd
  | BlitSaturate
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data PaintSource
  = PaintSolidColor !Color
  | PaintGradient !GradientType !Color !Color !GradientStopList

data GradientStop
  = GradientStop
    { theGradStopPoint :: !Float -- ^ A percentage value
    , theGradStopColor :: !Color
    }
  deriving Eq

data GradientType
  = GradLinear !(Angle Double)
  | GradRadial !(Point2D Float)
  deriving Eq

newtype GradientStopList = GradientStopList (UVec.Vector Word32)

gradStopPoint :: Lens' GradientStop Float
gradStopPoint = lens theGradStopPoint $ \ a b -> a{ theGradStopPoint = b }

gradStopColor :: Lens' GradientStop Color
gradStopColor = lens theGradStopColor $ \ a b -> a{ theGradStopColor = b }

gradStopsFromList :: [GradientStop] -> GradientStopList
gradStopsFromList lst = GradientStopList $ UVec.fromList words where
  stops = theGradStopPoint <$> lst
  lo = minimum stops
  hi = maximum stops
  r  = hi - lo
  normStops = (gradStopPoint %~ (\ p -> (p - lo) / r)) <$> lst
  words = do
    (GradientStop{theGradStopPoint=p, theGradStopColor=c}) <- normStops
    [floor $ p * realToFrac (maxBound :: Word32), get32BitsRGBA c]

gradStopsToList :: GradientStopList -> [GradientStop]
gradStopsToList (GradientStopList vec) = loop $ UVec.toList vec where
  loop = \ case
    p:c:more -> (: (loop more)) $ GradientStop
      { theGradStopPoint = realToFrac p / realToFrac (maxBound :: Word32)
      , theGradStopColor = set32BitsRGBA c
      }
    _ -> []

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
  -- 'pixel' ('Linear.V2.V2' 30 50) `'setVal'` 'red'
  --
  -- -- Get the color at point 45, 45
  -- @ 'getVal' $ 'pixel' $ 'Linear.V2.V2 45 45
  pixel :: Point2D SampCoord -> Variable render Color

  -- | This function should save the current drawing context, then evaluate a continuation function
  -- of type @render@, then restore the current drawing context
  tempContext :: render a -> render a

  -- | This function should restore the default graphics context state, deleting any 'shape's or
  -- 'fillPattern's currently set.
  resetGraphicsContext :: render ()

  -- | Fill the shape set by the 'shape' function in the 'Happlet2DGeometry' typeclass. If a 'shape'
  -- has not been set, blit the entire canvas with the 'fillColor' using the 'blitOperator'.
  fill :: render ()

  -- | Draw the 'Draw2DShape' function that was set by the 'shape' 'Variable'. If the 'shape' has
  -- not been set, then this function does nothing.
  stroke :: render ()

  -- | Set the blit operator used when drawing pixels with 'fill' and 'stroke'. The default operator
  -- should always be 'BlitSource'.
  blitOperator :: Variable render BlitOperator

  -- | Set the forground paint source, which could be a solid color or a gradient.
  fillColor :: Variable render PaintSource

  -- | Set the line paint source, which could be a solid color or a gradient.
  strokeColor :: Variable render PaintSource

  -- | Set the clipping rectangle. This is used when you want to restrict the drawing to a
  -- sub-region of the canvas. Note that this clip region will be a sub-region of the
  -- 'Happlets.GUI.windowClipRegion'.
  clipRegion :: Variable render (Rect2D SampCoord)

  -- | Set all pixels in the screen graphics to the same color value given by the 'fillColor',
  -- deleting all graphics that existed prior. This function does not reset the 'clipRegion'.
  clearScreen :: Happlet2DGraphics render => Color -> render ()
  clearScreen c = tempContext $ do
    resetGraphicsContext
    setVal fillColor $ PaintSolidColor c
    setVal blitOperator BlitSource
    fill

-- | The primitive functions given in this typeclass, namely the 'shape' drawing functions, should
-- install image drawing procedures into the @render@'s internal state and then the 'fill' and
-- 'stroke' functions in the 'Happelt2DGraphics' super-typeclass should execute these drawing
-- procedures to make updates to the operand canvas.
--
-- In order to do this, the @render@ function context needs to have a way of measuring pixels in
-- it's operand canvas, the "dimensions" of the pixels are therefore measured by some @dim@ type,
-- which will typically be 'Int', or perhaps 'Float' or 'Double' (as is the case with Cairo
-- Graphics). However, this class does not care what the @dim@ type is, only that it be instantiated
-- in conjunction with the @render@ function type.
class Happlet2DGraphics render => Happlet2DGeometry render dim where

  -- | Set the 'Draw2DShape' function to be used for a 'stroke' or 'fill' operation.
  shape :: Variable render (Draw2DShape dim)

  -- | Set the thickness or "weight" of the lines drawn by 'strokeLine', 'strokeRect', and
  -- 'strokeArc' functions.
  strokeWeight :: Variable render (LineWidth dim)

  -- | Before blitting, you can also set a transformation matrix that tells the blit operation to
  -- rotate, scale, translate, or skew, or any of the above.
  blitTransform :: Variable render (M44 dim)

-- | This class extends the 'Happlet2DGraphics' class with operators for copying sections of the
-- operand canvas into a pixel buffer.
class Happlet2DGraphics render => Happlet2DBuffersPixels render pixbuf where
  -- | This function is similar to the 'fillColor' function, but allows you to use another pixel
  -- buffer (a pattern), rather than a solid color or gradient, to be blitted to the canvas.
  fillPattern :: Variable render pixbuf

  -- | This function is similar to the 'strokeColor' function, but allows you to use another pixel
  -- buffer (a pattern), rather than a solid color or gradient, to be blitted to the canvas.
  strokePattern :: Variable render pixbuf

  -- | Create a copy of a portion of the canvas
  copyRegion :: Rect2D SampCoord -> render pixbuf

  -- | Create a new blank canvas
  newSubCanvas :: Size2D SampCoord -> render pixbuf

  -- | Evaluate a @render@ continuation function on a given @pixbuf@ canvas. When 'withSubCanvas'
  -- returns, drawing operations will continue to work on the main canvas.
  withSubCanvas :: pixbuf -> render a -> render a

-- | Calls @'getVal' 'pixel'@, applies a function @f@, then calls @'setVal' 'pixel'@ with the
-- result.
modifyPixel
  :: (Monad render, Happlet2DGraphics render)
  => Point2D SampCoord -> (Color -> Color) -> render ()
modifyPixel p f = getVal (pixel p) >>= setVal (pixel p) . f

----------------------------------------------------------------------------------------------------

-- | Provides a lens for changing the colour of various things.
class HasLineStyle a where { lineStyle :: Lens' (a num) (LineStyle num); }

data LineStyle num
  = LineStyle
    { theLineColor  :: !Color
    , theLineWeight :: !num
      -- ^ The weight specified in pixels
    }
  deriving (Eq, Show, Read)

instance HasLineStyle LineStyle where { lineStyle = lens id $ flip const; }

theLineColour :: LineStyle num -> Color
theLineColour = theLineColor

makeLineStyle :: Num num => LineStyle num
makeLineStyle = LineStyle
  { theLineColor  = packRGBA32 0xA0 0xA0 0xA0 0xA0
  , theLineWeight = 2
  }

lineColor :: HasLineStyle line => Lens' (line num) Color
lineColor = lineStyle . lens theLineColor (\ a b -> a{ theLineColor = b })

lineColour :: HasLineStyle line => Lens' (line num) Color
lineColour = lineColor

lineWeight :: HasLineStyle line => Lens' (line num) num
lineWeight = lineStyle . lens theLineWeight (\ a b -> a{ theLineWeight = b })

