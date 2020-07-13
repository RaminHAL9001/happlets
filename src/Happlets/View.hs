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
    Happlet2DGraphics(..), BlitOperator(..), modifyPixel,
    -- ** Fill Types
    PaintSource(..), GradientType(..), GradientStopList, GradientStop(..),
    gradStopsFromList, gradStopsToList, gradStopPoint, gradStopColor,
    -- * Geometric Shapes
    Happlet2DGeometry(..), Happlet2DHasDrawing(..), defaultClearScreen,
    -- ** Line Style
    HasLineStyle(..), LineStyle(..), theLineColour,
    makeLineStyle, lineColor, lineColour, lineWeight,
    -- * Image Buffers
    Happlet2DBuffersPixels(..), Happlet2DHasPattern(..), Source, Target,
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

import           Control.Lens (Lens', lens, (.=))
import           Control.Monad.IO.Class
import           Control.Monad.State.Class(MonadState)

import qualified Data.Vector.Unboxed as UVec
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Source image = image

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Target image = image

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
    { theGradStopPoint :: !Double -- ^ A percentage value
    , theGradStopColor :: !Color
    }
  deriving Eq

data GradientType
  = GradLinear !(Point2D Float) !(Point2D Float)
  | GradRadial !(Point2D Float) !(Magnitude Float) !(Point2D Float) !(Magnitude Float)
    -- ^ Specify two circles, the gradient will transition between these two circles.
  deriving Eq

newtype GradientStopList = GradientStopList (UVec.Vector Word32)

gradStopPoint :: Lens' GradientStop Double
gradStopPoint = lens theGradStopPoint $ \ a b -> a{ theGradStopPoint = b }

gradStopColor :: Lens' GradientStop Color
gradStopColor = lens theGradStopColor $ \ a b -> a{ theGradStopColor = b }

-- | Gradient stop points must be a percentage value, so 'gradStopPoint' values not between 0.0 and
-- 1.0 will be filtered from this list. The list of stops does not need to include the first and
-- last stop, the 'PaintGradient' data structure has two color values for storing these first and
-- last values.
gradStopsFromList :: [GradientStop] -> GradientStopList
gradStopsFromList lst = GradientStopList $ UVec.fromList words where
  valid stop = let p = theGradStopPoint stop in 0.0 <= p && p <= 1.0
  words = do
    (GradientStop{theGradStopPoint=p, theGradStopColor=c}) <- filter valid lst
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
  -- 'pixel' ('Linear.V2.V2' 30 50) `'setEnv'` 'red'
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

  -- | Fill the shape set by the 'shape' function in the 'Happlet2DGeometry' typeclass. If a 'shape'
  -- has not been set, blit the entire canvas with the 'fillColor' using the 'blitOperator'.
  fill :: render ()

  -- | Draw the 'Draw2DShape' function that was set by the 'shape' 'ConfigState'. If the 'shape' has
  -- not been set, then this function does nothing.
  stroke :: render ()

  -- | Set all pixels in the screen graphics to the same color value given by the 'fillColor',
  -- deleting all graphics that existed prior. This function does not reset the 'clipRegion'.
  clearScreen :: Happlet2DGraphics render => Color -> render ()

class Happlet2DHasDrawing renderState where

  -- | Set the clipping rectangle. This is used when you want to restrict the drawing to a
  -- sub-region of the canvas. Note that this clip region will be a sub-region of the
  -- 'Happlets.GUI.windowClipRegion'.
  clipRegion :: Lens' renderState (Rect2D SampCoord)

  -- | Set the blit operator used when drawing pixels with 'fill' and 'stroke'. The default operator
  -- should always be 'BlitSource'.
  blitOperator :: Lens' renderState BlitOperator

  -- | Set the forground paint source, which could be a solid color or a gradient.
  fillColor :: Lens' renderState PaintSource

  -- | Set the line paint source, which could be a solid color or a gradient.
  strokeColor :: Lens' renderState PaintSource

-- | This function provides a default implementation for 'clearScreen' for the 'Happlet2DGraphics'
-- typeclass, as long as the @render@ function that instantiates the 'Happlet2DGraphics' typeclass
-- also instantiates the 'Happle2DHasDrawing' typeclass as well.
defaultClearScreen
  :: (MonadState renderState render, Happlet2DHasDrawing renderState, Happlet2DGraphics render)
  => Color -> render ()
defaultClearScreen c = tempContext $ do
  resetGraphicsContext
  fillColor    .= PaintSolidColor c
  blitOperator .= BlitSource
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
class Happlet2DGeometry renderState dim where

  -- | Set the 'Draw2DShape' function to be used for a 'stroke' or 'fill' operation.
  shape :: Lens' renderState (Draw2DShape dim)

  -- | Set the thickness or "weight" of the lines drawn by 'strokeLine', 'strokeRect', and
  -- 'strokeArc' functions.
  strokeWeight :: Lens' renderState (LineWidth dim)

  -- | Before blitting, you can also set a transformation matrix that tells the blit operation to
  -- rotate, scale, translate, or skew, or any of the above.
  blitTransform :: Lens' renderState (Transform2D dim)

  -- | If the 'PaintSource' is a gradient, or perhaps a pixel buffer given by the 'fillPattern'
  -- 'ConfigState', then you may want to apply a transformation to the pattern before blitting.
  patternTransform :: Lens' renderState (Transform2D dim)

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

class Happlet2DHasPattern renderState pixbuf where
  -- | This function is similar to the 'fillColor' function, but allows you to use another pixel
  -- buffer (a pattern), rather than a solid color or gradient, to be blitted to the canvas.
  fillPattern :: Lens' renderState pixbuf

  -- | This function is similar to the 'strokeColor' function, but allows you to use another pixel
  -- buffer (a pattern), rather than a solid color or gradient, to be blitted to the canvas.
  strokePattern :: Lens' renderState pixbuf


-- | Calls @'getEnv' 'pixel'@, applies a function @f@, then calls @'setEnv' 'pixel'@ with the
-- result.
modifyPixel
  :: (Monad render, Happlet2DGraphics render)
  => Point2D SampCoord -> (Color -> Color) -> render ()
modifyPixel p f = getConfig (pixel p) >>= setConfig (pixel p) . f

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

