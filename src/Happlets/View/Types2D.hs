-- | This module provides a bare minimum of 2D drawing primitives for drawing text, lines, and
-- rectangles, along with setting foreground and background colors, and fill and stroke colors. Also
-- provided are functions for determining if points (for example, mouse cursor locations) lie within
-- a rectangular region. These primitives, along with the functionality in the
-- 'Happlets.GUI.CanBufferImages' type class to load from disk and blit to screen bitmap images,
-- should be enough to construct minimalist user interfaces.
module Happlets.View.Types2D
  ( -- * Sample Coordinate
    -- 
    -- The "samples" are the integer valued locations of pixels on a display, and are used wherever
    -- a dimensions of a graphical primitive is specified as an integer multiple of the width of a
    -- display pixel.
    SampCoord, PixCoord, PixSize, sampCoord,
    -- * Typeclasses
    Has2DOrigin(..), Is2DPrimitive(..),
    -- * Primitives
    Draw2DPrimitive(..), Map2DShape(..),
    -- ** Points
    Point2D, Size2D,
    point2D, size2D, pointX, pointY, pointXY,
    -- ** Lines
    Line2D(..), line2D, line2DHead, line2DTail, line2DPoints,
    LineWidth,
    -- *** Line Style
    HasLineStyle(..), LineStyle(..), theLineColour,
    makeLineStyle, lineColor, lineColour, lineWeight,
    -- ** Rectangles
    Rect2D(..), rect2D, rect2DSize, rect2DHead, rect2DTail, pointInRect2D, rect2DPoints,
    rect2DCenter, rect2DCentre,
    canonicalRect2D, rect2DMinBoundOf, rect2DIntersect, rect2DDiagonal, rect2DtoInt,
    MaybeSingleton2D(..), HasBoundingBox(..),
    -- ** Arcs
    Magnitude(..), ArcRadius, Angle(..), StartAngle, EndAngle,
    Arc2D(..), arc2D, arc2DOrigin, arc2DRadius, arc2DStart, arc2DEnd,
    -- ** Paths
    Path2D, path2D, path2DOrigin, path2DPoints,
    -- ** Cubic Bezier Spline Paths
    Cubic2D, Cubic2DSegment(..), cubic2D, cubic2DOrigin, cubic2DPoints,
    cubic2DCtrlPt1, cubic2DCtrlPt2, cubic2DEndPoint,
    -- ** Matrix Transformations
    BoundingBox2D(..), boxBounds2D, boxTransform2D, boxModel2D,
    Transform2D(..), idTrans2D, transform2D, trans2DDraw,
    -- ** Fill Types
    Draw2DFillStroke(..),
    BlitOperator(..),
    PaintSource(..), PaintSourceFunction(..),
    paintColor, paintSourceBlit, paintSourceFunction,
    PaintGradient(..),
    paintGradType, paintGradStartColor, paintGradEndColor, paintGradStopList,
    GradientType(..), GradientStopList, GradientStop(..),
    gradStopsFromList, gradStopsToList, gradStopPoint, gradStopColor,
    -- * Re-exports
    module Linear.V2,
    module Linear.Matrix,
  ) where

import           Happlets.View.Color

import           Control.Arrow
import           Control.Lens
import           Control.Monad

import           Data.Int            (Int32)
import qualified Data.Vector.Unboxed as UVec
import           Data.Word           (Word32)

import           Linear.V2
import           Linear.Matrix

----------------------------------------------------------------------------------------------------

-- | A sample coordinate: this is a value that is used to count some integral number of units along
-- a 1-dimensional axis of some scan line, for example pixels along the edge of a view screen or
-- canvas. Alone, these elements can represent the amount of time (measured in the units of time
-- samples) of an audio signal. A 'Linear.V2.V2' pair's of 'PixCoords' can specify a 2D point on a
-- screen or canvas, 'Linear.V3.V3' triples of 'PixCoords' can specify a 3D point in a voxel space.
type SampCoord = Int32

-- | A pair of 'SampCoord' indicating the location of a pixel in a raster image.
type PixCoord = V2 SampCoord

-- | A pair of 'SampCoord' values indicating the size of a rectangle of pixels.
type PixSize = V2 SampCoord

sampCoord :: Integral i => i -> SampCoord
sampCoord = fromIntegral

----------------------------------------------------------------------------------------------------

-- | Some of the shapes in this module cannot instantiate 'Functor' because there are constrains on
-- the numerical type used to define the shape, namely 'RealFrac' and 'UVec.Unbox'.
class Map2DShape shape where
  map2DShape :: (Real n,  UVec.Unbox n) => (n -> n) -> shape n -> shape n

instance Map2DShape V2 where { map2DShape = fmap; }

-- | This type represents a single point.
type Point2D n = V2 n

type Size2D n = V2 n

-- | An initializing 'Point2D' where 'pointX' and 'pointY' are both zero.
point2D :: Num n => Point2D n
point2D = V2 0 0

-- | A synonym for 'point2D'.
size2D :: Num n => Size2D n
size2D = point2D

-- | The X coordinate of a 'Point2D', a value expressing some distance along the horizontal axis.
pointX :: Lens' (Point2D n) n
pointX = lens (\ (V2 x _) -> x) $ \ (V2 _ y) x -> (V2 x y)

-- | The Y coordinate of a 'Point2D', a value expressing some distance along the horizontal axis.
pointY :: Lens' (Point2D n) n
pointY = lens (\ (V2 _ y) -> y) $ \ (V2 x _) y -> (V2 x y)

-- | Expresses the point as a tuple of @(x, y)@ coordinates
pointXY :: Iso' (Point2D n) (n, n)
pointXY = iso (\ (V2 x y) -> (x, y)) (\ (x,y) -> V2 x y)

----------------------------------------------------------------------------------------------------

-- | Values of this type are used when drawing lines or borders around rectangls.
type LineWidth n = n

-- | A matrix of type 'M44' which is used to construct transformations. Although the name of this
-- type implies a 2D transformation, it can actually transform along 4 dimensions, making it easier
-- to setup transformation matrixies according to the 'mkTransformationMat' function.
data Transform2D n drawing
  = Transform2D
    { theTransform2D :: !(M44 n)
    , theDrawing2D :: drawing
    }
  deriving (Eq, Functor)

map2DTransform :: (n -> m) -> Transform2D n drawing -> Transform2D m drawing
map2DTransform f (Transform2D{theTransform2D=m44,theDrawing2D=draw}) =
  Transform2D
  { theTransform2D = fmap f <$> m44
  , theDrawing2D   = draw
  }

-- | The identity transformation
idTrans2D :: Num n => drawing -> Transform2D n drawing
idTrans2D drawing = Transform2D{ theTransform2D = identity, theDrawing2D = drawing }

transform2D :: Lens' (Transform2D n drawing) (M44 n)
transform2D = lens theTransform2D $ \ a b -> a{ theTransform2D = b }

trans2DDraw :: Lens' (Transform2D n drawing) drawing
trans2DDraw = lens theDrawing2D $ \ a b -> a{ theDrawing2D = b }

----------------------------------------------------------------------------------------------------

-- | A free-floating 'Widget' ("F.F. Widget") is a 'Widget' that is not constrained by tiling or
-- grid rules, it may exist anywhere on the canvas.
data BoundingBox2D trans n model
  = BoundingBox2D
    { theBoxBounds2D :: !(Transform2D trans (Rect2D n))
    , theBoxModel2D  :: !model
    }
  deriving Functor

boxBoundsTrans2D :: Lens' (BoundingBox2D trans n model) (Transform2D trans (Rect2D n))
boxBoundsTrans2D = lens theBoxBounds2D $ \ a b -> a{ theBoxBounds2D = b }

-- | Operate on the bounding box and transformation of the 'StagedWidget'.
boxBounds2D :: Lens' (BoundingBox2D trans n model) (Rect2D n)
boxBounds2D = boxBoundsTrans2D . trans2DDraw

-- | Bounding boxes can also have a linear transformation applied, which can be apply to the @model@
-- as when it is rendered to a canvas.
boxTransform2D :: Lens' (BoundingBox2D trans n model) (M44 trans)
boxTransform2D = boxBoundsTrans2D . transform2D

-- | Operate on the 'Widget' of the 'StagedWidget'.
boxModel2D :: Lens' (BoundingBox2D trans n model) model
boxModel2D = lens theBoxModel2D $ \ a b -> a{ theBoxModel2D = b }

----------------------------------------------------------------------------------------------------

class HasBoundingBox a where
  type Bounds2DMetric a
  theBoundingBox :: a -> Rect2D (Bounds2DMetric a)

instance HasBoundingBox (Rect2D n) where
  type Bounds2DMetric (Rect2D n) = n
  theBoundingBox = id; 

instance HasBoundingBox (Line2D n) where
  type Bounds2DMetric (Line2D n) = n
  theBoundingBox (Line2D a b) = Rect2D a b; 

instance HasBoundingBox (BoundingBox2D trans n model) where
  type Bounds2DMetric (BoundingBox2D trans n model) = n
  theBoundingBox = view boxBounds2D; 

----------------------------------------------------------------------------------------------------

-- | This type represents a line segment consisting of two points.
data Line2D n = Line2D !(Point2D n) !(Point2D n)
  deriving (Eq, Functor)

instance Map2DShape Line2D where { map2DShape = fmap; }

-- | An initializing 'Line2D' where 'lineHead' and 'lineTail' are both the zero 'point2D'.
line2D :: Num n => Line2D n
line2D = Line2D point2D point2D

-- | The point at which drawing of a line segment begins.
line2DTail :: Lens' (Line2D n) (Point2D n)
line2DTail = lens (\ (Line2D a _) -> a) $ \ (Line2D _ b) a -> Line2D a b

-- | The point at which drawing of a line segment ends.
line2DHead :: Lens' (Line2D n) (Point2D n)
line2DHead = lens (\ (Line2D _ b) -> b) $ \ (Line2D a _) b -> Line2D a b

-- | Expresses a 'Line2D' as a tuple of 'Point2D' values.
line2DPoints :: Iso' (Line2D n) (Point2D n, Point2D n)
line2DPoints = iso (\ (Line2D a b) -> (a, b)) (\ (a,b) -> Line2D a b)

----------------------------------------------------------------------------------------------------

-- | This type represents a rectangle bounded by two where all sides of the rectangle points are
-- parallel to the orthogonal basis vectors of the drawing plane. This data type instantiates
-- 'Data.Semigroup.Semigroup' such that the smallest possible rectangle that can contain both
-- rectangles, no matter how far apart they are, is returned.
data Rect2D n = Rect2D !(Point2D n) !(Point2D n)
  deriving (Eq, Functor)

instance Ord n => Semigroup (Rect2D n) where { (<>) = rect2DMinBoundOf; }
instance Map2DShape Rect2D where { map2DShape = fmap; }

-- | An initializing 'Line2D' where the 'rectLower' and 'rectUpper' values are the zero 'point2D'.
rect2D :: Num n => Rect2D n
rect2D = Rect2D point2D point2D

-- | The point at which drawing of a bounding rectangle begins.
rect2DTail :: Lens' (Rect2D n) (Point2D n)
rect2DTail = lens (\ (Rect2D tail _) -> tail) $ \ (Rect2D _ head) tail -> Rect2D tail head

-- | The point at which drawing of a bounding rectangle ends.
rect2DHead :: Lens' (Rect2D n) (Point2D n)
rect2DHead = lens (\ (Rect2D _ head) -> head) $ \ (Rect2D tail _) head -> Rect2D tail head

-- | The width and height of a 'Rect2D'. Note that this is not a lens, since there are multiple
-- possible ways to "set" the size of a 'Rect2D'.
rect2DSize :: Num n => Rect2D n -> Size2D n
rect2DSize (Rect2D tail head) = head - tail

-- | Computes the center point of the given 'Rect2D'.
rect2DCenter :: Fractional n => Rect2D n -> Point2D n
rect2DCenter r = ((/ 2) <$> rect2DSize r) + (r ^. rect2DHead)

-- | British spelling of 'rect2DCenter'.
rect2DCentre :: Fractional n => Rect2D n -> Point2D n
rect2DCentre = rect2DCenter

-- | Expresses a 'Rect2D' as a tuple of 'Point2D' values.
rect2DPoints :: Iso' (Rect2D n) (Point2D n, Point2D n)
rect2DPoints = iso (\ (Rect2D a b) -> (a, b)) (\ (a,b) -> Rect2D a b)

-- | Re-order the bounding 'Point2D's of the 'Rect2D' such that the 'rect2DHead' is the point
-- closest to the origin @('Linear.V2.V2' 0 0)@ and the 'rect2DTail' is the point furthest from the
-- origin.
canonicalRect2D :: Ord n => Rect2D n -> Rect2D n
canonicalRect2D (Rect2D (V2 x0 y0) (V2 x1 y1)) =
  Rect2D (V2 (min x0 x1) (min y0 y1)) (V2 (max x0 x1) (max y0 y1))

-- | Test if the given 'Point2D' lies within, or on the bounding box of, the given 'Rect2D'.
pointInRect2D :: Ord n => Point2D n -> Rect2D n -> Bool
pointInRect2D (V2 x y) = view rect2DPoints . canonicalRect2D >>> \ (p0, p1) ->
  let (xlo, ylo) = p0 ^. pointXY
      (xhi, yhi) = p1 ^. pointXY
      between a b c = a <= b && b <= c
  in  between xlo x xhi && between ylo y yhi

-- | Computes the smallest possible rectangle that can contain both rectangles, no matter how far
-- apart they are.
rect2DMinBoundOf :: Ord n => Rect2D n -> Rect2D n -> Rect2D n
rect2DMinBoundOf (Rect2D(V2 xa ya)(V2 xb yb)) (Rect2D(V2 xc yc)(V2 xd yd)) =
  let f4 comp a b c d = comp a $ comp b $ comp c d in Rect2D
    (V2 (f4 min xa xb xc xd) (f4 min ya yb yc yd))
    (V2 (f4 max xa xb xc xd) (f4 max ya yb yc yd))

-- | Returns an intersection of two 'Rect2D's if the two 'Rect2D's overlap.
rect2DIntersect :: Ord n => Rect2D n -> Rect2D n -> Maybe (Rect2D n)
rect2DIntersect
  (Rect2D(V2 xa ya)(V2 xb yb))
  (Rect2D(V2 xc yc)(V2 xd yd)) = do
    (xa, xb) <- pure (min xa xb, max xa xb)
    (ya, yb) <- pure (min ya yb, max ya yb)
    (xc, xd) <- pure (min xc xd, max xc xd)
    (yc, yd) <- pure (min yc yd, max yc yd)
    let between a b c = a <= b && b <= c
    let isect a b c d = guard (between a c b || between c b d) >> return (min xa xc, max xb xd) 
    (xlo, xhi) <- isect xa xb xc xd
    (ylo, yhi) <- isect ya yb yc yd
    return $ Rect2D (V2 xlo ylo) (V2 xhi yhi)

-- | Convert a 'Rect2D' to a 'Line2D'
rect2DDiagonal :: Rect2D n -> Line2D n
rect2DDiagonal (Rect2D a b) = Line2D a b

-- | Evaluate 'floor' on the 'rect2DHead and 'ceiling' on the 'rect2DTail'.
rect2DtoInt :: (RealFrac n, Integral i) => Rect2D n -> Rect2D i
rect2DtoInt r = rect2D
  & rect2DTail .~ (floor <$> (r ^. rect2DTail))
  & rect2DHead .~ (ceiling <$> (r ^. rect2DHead))

----------------------------------------------------------------------------------------------------

newtype Magnitude n = Magnitude n
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, RealFrac, Floating, Functor)

type ArcRadius n = Magnitude n

newtype Angle n = Angle n
  deriving (Eq, Ord, Show, Read, Num, Real, Fractional, RealFrac, Floating, Functor)

type StartAngle n = Angle n
type EndAngle n   = Angle n

data Arc2D n
  = Arc2D
    { theArc2DOrigin :: !(Point2D n)
    , theArc2DRadius :: !(Magnitude n)
    , theArc2DStart  :: !(StartAngle Double)
    , theArc2DEnd    :: !(EndAngle Double)
    }
  deriving (Eq, Functor)

instance Map2DShape Arc2D where { map2DShape = fmap; }

arc2D :: Num n => Arc2D n
arc2D = Arc2D
  { theArc2DOrigin = V2 0 0
  , theArc2DRadius = 1
  , theArc2DStart  = 0.0
  , theArc2DEnd    = 2.0 * pi
  }

arc2DOrigin :: Lens' (Arc2D n) (Point2D n)
arc2DOrigin = lens theArc2DOrigin $ \ a b -> a{ theArc2DOrigin = b }

arc2DRadius :: Lens' (Arc2D n) (Magnitude n)
arc2DRadius = lens theArc2DRadius $ \ a b -> a{ theArc2DRadius = b }

arc2DStart :: Lens' (Arc2D n) (StartAngle Double)
arc2DStart = lens theArc2DStart $ \ a b -> a{ theArc2DStart = b }

arc2DEnd :: Lens' (Arc2D n) (EndAngle Double)
arc2DEnd = lens theArc2DEnd $ \ a b -> a{ theArc2DEnd = b }

----------------------------------------------------------------------------------------------------

data Path2D n = Path2D !(Point2D n) !(UVec.Vector n)
  deriving Eq

instance Map2DShape Path2D where
  map2DShape f (Path2D p vec) = Path2D (f <$> p) (UVec.map f vec)

path2DOrigin :: Lens' (Path2D n) (Point2D n)
path2DOrigin = lens (\ (Path2D p _) -> p) (\ (Path2D _ v) p -> Path2D p v)

-- | Construct a new 'Path2D'
path2D :: UVec.Unbox n => Point2D n -> [Point2D n] -> Path2D n
path2D p = Path2D p . UVec.fromList . (>>= (\ (V2 x y) -> [x, y]))

-- | Deconstruct a 'Path2D' into it's component points
path2DPoints :: UVec.Unbox n => Path2D n -> (Point2D n, [Point2D n])
path2DPoints (Path2D p v) = (p, points id (UVec.toList v)) where
  points stack = \ case
    x:y:more -> points (stack . ((V2 x y) :)) more
    _        -> stack []

----------------------------------------------------------------------------------------------------

data Cubic2D n = Cubic2D !(Point2D n) !(UVec.Vector n)
  deriving Eq

instance Map2DShape Cubic2D where
  map2DShape f (Cubic2D p vec) = Cubic2D (f <$> p) (UVec.map f vec)

data Cubic2DSegment n
  = Cubic2DSegment
    { theCubic2DCtrlPt1 :: !(Point2D n)
    , theCubic2DCtrlPt2 :: !(Point2D n)
    , theCubic2DEndPoint :: !(Point2D n)
    }
  deriving (Eq, Functor)

instance Map2DShape Cubic2DSegment where { map2DShape = fmap; }


cubic2DOrigin :: Lens' (Cubic2D n) (Point2D n)
cubic2DOrigin = lens (\ (Cubic2D p _) -> p) (\ (Cubic2D _ v) p -> Cubic2D p v)

-- | Construct a new 'Cubic2D'
cubic2D :: UVec.Unbox n => Point2D n -> [Cubic2DSegment n] -> Cubic2D n
cubic2D p = Cubic2D p . UVec.fromList .
  (>>= (\ (Cubic2DSegment (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) -> [x1, y1, x2, y2, x3, y3]))

-- | Deconstruct a 'Cubic2D' into it's component points
cubic2DPoints :: UVec.Unbox n => Cubic2D n -> (Point2D n, [Cubic2DSegment n])
cubic2DPoints (Cubic2D p v) = (p, points id (UVec.toList v)) where
  points stack = \ case
    x1:y1:x2:y2:x3:y3:more ->
      points (stack . ((Cubic2DSegment (V2 x1 y1) (V2 x2 y2) (V2 x3 y3)) :)) more
    _                      -> stack []

cubic2DCtrlPt1 :: Lens' (Cubic2DSegment n) (Point2D n)
cubic2DCtrlPt1 = lens theCubic2DCtrlPt1 $ \ a b -> a{ theCubic2DCtrlPt1 = b }

cubic2DCtrlPt2 :: Lens' (Cubic2DSegment n) (Point2D n)
cubic2DCtrlPt2 = lens theCubic2DCtrlPt1 $ \ a b -> a{ theCubic2DCtrlPt2 = b }

cubic2DEndPoint :: Lens' (Cubic2DSegment n) (Point2D n)
cubic2DEndPoint = lens theCubic2DEndPoint $ \ a b -> a{ theCubic2DEndPoint = b }

----------------------------------------------------------------------------------------------------

data Draw2DPrimitive n
  = Draw2DReset
    -- ^ If 'fill' is called, this function sets all pixels in the buffer to the 'fillColor' value.
  | Draw2DLine  !(PaintSource n)      !(Line2D n)
  | Draw2DRect  !(Draw2DFillStroke n) !(Rect2D n)
  | Draw2DArc   !(Draw2DFillStroke n) !(Arc2D  n)
  | Draw2DPath  !(Draw2DFillStroke n) !(Path2D n)
  | Draw2DCubic !(Draw2DFillStroke n) !(Cubic2D n) -- ^ A cubic Bezier spline
  deriving Eq

instance Map2DShape Draw2DPrimitive where
  map2DShape f = \ case
    Draw2DReset             -> Draw2DReset
    Draw2DLine  paint shape -> Draw2DLine  (map2DShape f paint) (map2DShape f shape)
    Draw2DRect  paint shape -> Draw2DRect  (map2DShape f paint) (map2DShape f shape)
    Draw2DArc   paint shape -> Draw2DArc   (map2DShape f paint) (map2DShape f shape)
    Draw2DPath  paint shape -> Draw2DPath  (map2DShape f paint) (map2DShape f shape)
    Draw2DCubic paint shape -> Draw2DCubic (map2DShape f paint) (map2DShape f shape)

class Is2DPrimitive shape where
  to2DShape :: Draw2DFillStroke n -> shape n -> Draw2DPrimitive n

instance Is2DPrimitive Line2D  where
  to2DShape = Draw2DLine . \ case
    FillOnly   paint   -> paint
    StrokeOnly paint   -> paint
    FillStroke _ paint -> paint
    StrokeFill paint _ -> paint

instance Is2DPrimitive Arc2D   where { to2DShape = Draw2DArc; }
instance Is2DPrimitive Rect2D  where { to2DShape = Draw2DRect; }
instance Is2DPrimitive Path2D  where { to2DShape = Draw2DPath; }
instance Is2DPrimitive Cubic2D where { to2DShape = Draw2DCubic; }

class Has2DOrigin shape where
  origin2D :: Lens' (shape n) (Point2D n)

instance Has2DOrigin Arc2D   where { origin2D = arc2DOrigin; }
instance Has2DOrigin Line2D  where { origin2D = line2DHead; }
instance Has2DOrigin Rect2D  where { origin2D = rect2DHead; }
instance Has2DOrigin Path2D  where { origin2D = path2DOrigin; }
instance Has2DOrigin Cubic2D where { origin2D = cubic2DOrigin; }

----------------------------------------------------------------------------------------------------

-- | A class providing 'isSingleton2D' which whether a 'Rect2D' or a 'Line2D' are constructed from
-- two identical points. There are theoretically an infinite number of singletons in the 2D
-- plane. The 'line2D' and 'rect2D' constructors are singletons as they both start and end at the
-- zero 'point2D' value.
class MaybeSingleton2D a where { isSingleton2D :: Eq n => a n -> Maybe (Point2D n); }

instance MaybeSingleton2D Line2D where
  isSingleton2D (Line2D a b) = if a == b then Just a else Nothing

instance MaybeSingleton2D Rect2D where
  isSingleton2D (Rect2D a b) = if a == b then Just a else Nothing

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

data PaintSource n
  = PaintSource
    { thePaintSourceBlit     :: !BlitOperator
    , thePaintSourceFunction :: !(PaintSourceFunction n)
    }
  deriving Eq

instance Map2DShape PaintSource where
  map2DShape f (PaintSource{thePaintSourceBlit=blit,thePaintSourceFunction=src}) =
    PaintSource
    { thePaintSourceBlit     = blit
    , thePaintSourceFunction = map2DShape f src
    }

paintColor :: Color -> PaintSource n
paintColor c = PaintSource
  { thePaintSourceBlit = BlitSource
  , thePaintSourceFunction = SolidColorSource c
  }

paintSourceBlit :: Lens' (PaintSource n) BlitOperator
paintSourceBlit = lens thePaintSourceBlit $ \ a b -> a{ thePaintSourceBlit = b }

paintSourceFunction :: Lens' (PaintSource n) (PaintSourceFunction n)
paintSourceFunction = lens thePaintSourceFunction $ \ a b -> a{ thePaintSourceFunction = b }

----------------------------------------------------------------------------------------------------

data PaintSourceFunction n
  = SolidColorSource  !Color
  | GradientSource    !(Transform2D n PaintGradient)
  | PixelBufferSource () -- TODO
  deriving Eq

instance Map2DShape PaintSourceFunction where
  map2DShape f = \ case
    SolidColorSource   o -> SolidColorSource o
    GradientSource     o -> GradientSource $ map2DTransform f o
    PixelBufferSource () -> PixelBufferSource ()

data PaintGradient
  = PaintGradient
    { thePaintGradType       :: !GradientType
    , thePaintGradStartColor :: !Color
    , thePaintGradEndColor   :: !Color
    , thePaintGradStopList   :: !GradientStopList
    }
  deriving Eq

paintGradType :: Lens' PaintGradient GradientType
paintGradType = lens thePaintGradType $ \ a b -> a{ thePaintGradType = b }

paintGradStartColor :: Lens' PaintGradient Color
paintGradStartColor = lens thePaintGradStartColor $ \ a b -> a{ thePaintGradStartColor = b }

paintGradEndColor :: Lens' PaintGradient Color
paintGradEndColor = lens thePaintGradEndColor $ \ a b -> a{ thePaintGradEndColor = b }

paintGradStopList :: Lens' PaintGradient GradientStopList
paintGradStopList = lens thePaintGradStopList $ \ a b -> a{ thePaintGradStopList = b }

data GradientStop
  = GradientStop
    { theGradStopPoint :: !Double -- ^ A percentage value
    , theGradStopColor :: !Color
    }
  deriving Eq

gradStopPoint :: Lens' GradientStop Double
gradStopPoint = lens theGradStopPoint $ \ a b -> a{ theGradStopPoint = b }

gradStopColor :: Lens' GradientStop Color
gradStopColor = lens theGradStopColor $ \ a b -> a{ theGradStopColor = b }

data GradientType
  = GradLinear !(Point2D Float) !(Point2D Float)
  | GradRadial !(Point2D Float) !(Magnitude Float) !(Point2D Float) !(Magnitude Float)
    -- ^ Specify two circles, the gradient will transition between these two circles.
  deriving Eq

newtype GradientStopList = GradientStopList (UVec.Vector Word32)
  deriving Eq

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

----------------------------------------------------------------------------------------------------

-- | Most 'Draw2DPrimitive's enclose an 2D region within a path that can be filled, as well as
-- having a border line drawn along the enclosing path, as though a brush were stroking some paint
-- along the path. This function specifies the order of fill and stroke operations to perform.
data Draw2DFillStroke n
  = FillOnly (PaintSource n)
  | StrokeOnly (PaintSource n)
  | FillStroke (PaintSource n) (PaintSource n)
  | StrokeFill (PaintSource n) (PaintSource n)
  deriving Eq

instance Map2DShape Draw2DFillStroke where
  map2DShape f = \ case
    FillOnly   a   -> FillOnly   (map2DShape f a)
    StrokeOnly b   -> StrokeOnly (map2DShape f b)
    FillStroke a b -> FillStroke (map2DShape f a) (map2DShape f b)
    StrokeFill b a -> StrokeFill (map2DShape f b) (map2DShape f a)
