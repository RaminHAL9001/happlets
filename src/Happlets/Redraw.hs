module Happlets.Redraw where

import           Data.Int
import           Data.Semigroup

import           Linear.V2

----------------------------------------------------------------------------------------------------

-- | A sample coordinate: this is a value that is used to count some integral number of units along
-- a 1-dimensional axis of some scan line, for example pixels along the edge of a view screen or
-- canvas. Alone, these elements can represent the amount of time (measured in the units of time
-- samples) of an audio signal. A 'Linear.V2.V2' pair's of 'PixCoords' can specify a 2D point on a
-- screen or canvas, 'Linear.V3.V3' triples of 'PixCoords' can specify a 3D point in a voxel space.
newtype SampCoord = SampCoord Int32
  deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

-- | A pair of 'SampCoord' indicating the location of a pixel in a raster image.
type PixCoord = V2 SampCoord

-- | A pair of 'SampCoord' values indicating the size of a rectangle of pixels.
type PixSize = V2 SampCoord

sampCoord :: Integral i => i -> SampCoord
sampCoord = SampCoord . fromIntegral

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Source image = image

-- | This is a simple type tag used to indicate the difference between a source and target image,
-- for operations like blitting.
type Target image = image

-- | This data type specifies instructions on how to redraw the content of a Happlet window in
-- response to events. Happlet drawing is very stateful and straight forward: whatever you draw to
-- the canvas stays there until you draw over it with something else.
--
-- The @render@ type is used by the 'Happlets.GUI.onView' function of the
-- 'Happlets.GUI.HappletWindow' type class, and will almost certainly be a monadic function type, or
-- at least an applicative function type. The type @a@ is the functor of this applicative type. The
-- 'Redraw' data type as a whole instantiates 'Data.Monoid.Monoid' so that redrawing operations can
-- be accumulated before being evaluated to an image-updating @IO@ function type.
--
-- To go into more detail: every Happlet GUI back-end /must/ double-buffer the canvas by maintaining
-- an off-screen buffer which receives the updates called by the Happlet 'Agent'. The Happlet GUI
-- back-end /must/ also copy regions of this off-screen buffer to the operating system's When a
-- window needs to be redrawn, the Happlet back-end will simply re-copy this buffer back to the
-- operating system's screen buffer. This means all drawing operations are performed on the
-- off-screen canvas buffer and are then flushed to the operating system screen buffer at the end of
-- each round of executing the event handler callback.
data Redraw render a
  = NoRedraw !WindowVisibility
    -- ^ Indicate that the off-screen canvas must not be updated. It may also indicate to hide the
    -- window. The 'noRedraw' function specifies @'NoRedraw' 'WindowVisible'@.
  | Redraw
    { redrawRegions  :: [ScreenMask]
    , redrawFunction :: PixCoord -> render a
    }
    -- ^ 'Redraw' implies 'WindowVisible'. This data type must contain a list of regions that were
    -- redrawn, which will instruct to the Happlet back-end which portion of the off-screen canvas
    -- to be flushed to the operating system's screen buffer.

-- | This is essentially a boolean type specifying whether the Happlet window should be hidden or
-- visible.
data WindowVisibility = WindowVisible | WindowHidden
  deriving (Eq, Ord, Show, Read, Enum)

-- | A simple language for specifying regions of a canvas to be updated. Think of these as
-- instructions for drawing to a black-and-white bitmap mask, and wherever a bit has been drawn, an
-- an update will occur. A 'Redraw' data constructor will contain a list of these operations.
--
-- Note that not all GUI back-ends may support all of these mask types, at the very minimum,
-- 'ScreenRect' without rotation angles should be supported.
data ScreenMask
  = ScreenAll -- ^ Mask the whole screen.
  | ScreenRect       !PixCoord  !PixCoord
    -- ^ Rectangle upper-left corner and lower-right corner
  | ScreenOval       !PixCoord  !SampCoord !SampCoord
    -- ^ Oval with origin point, width, height
  | ScreenTriangle   !PixCoord  !PixCoord  !PixCoord
    -- ^ Triangle bound by three points.
  | ScreenVertical   !SampCoord !SampCoord !SampCoord
    -- ^ A vertical line, first parameter specifying the distance from the left of the canvas,
    -- second parameter specifying the distance from the top of the canvas where the line begins,
    -- the third parameter specifying the distance from the bottom of the canvas where the line
    -- ends.
  | ScreenHorizontal !SampCoord !SampCoord !SampCoord
    -- ^ The transpose of 'ScreenVertical'
  deriving (Eq, Ord, Show, Read)

-- | Returns 'Prelude.True' if the list contains a 'ScreenAll' constructor.
masksAll :: [ScreenMask] -> Bool
masksAll = or . fmap (== ScreenAll)

instance (Applicative render, Semigroup a) => Semigroup (Redraw render a) where
    (<>) = appendDraw

instance (Applicative render, Semigroup a) => Monoid (Redraw render a) where
  mempty  = noRedraw
  mappend = appendDraw

appendDraw
  :: (Applicative render, Semigroup a)
  => Redraw render a -> Redraw render a -> Redraw render a
appendDraw a b = case a of
  NoRedraw a -> case b of
    NoRedraw b -> NoRedraw (min a b)
    b          -> b
  Redraw{ redrawRegions=aR, redrawFunction=aF } -> case b of
    NoRedraw{} -> a
    Redraw{ redrawRegions=bR, redrawFunction=bF } ->
      Redraw
      { redrawRegions  = aR ++ bR
      , redrawFunction = \ sz -> (<>) <$> aF sz <*> bF sz
      }

-- | Indicate the window should remain visible but not be redrawn.
noRedraw :: Redraw render any
noRedraw = NoRedraw WindowVisible

-- | Indicate that the whole window should be redrawn.
redrawAll :: (PixCoord -> render a) -> Redraw render a
redrawAll f = Redraw{ redrawRegions = [ScreenAll], redrawFunction = f }
