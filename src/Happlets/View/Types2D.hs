-- | This module provides a bare minimum of 2D drawing primitives for drawing text, lines, and
-- rectangles, along with setting foreground and background colors, and fill and stroke colors. Also
-- provided are functions for determining if points (for example, mouse cursor locations) lie within
-- a rectangular region. These primitives, along with the functionality in the
-- 'Happlets.GUI.CanBufferImages' type class to load from disk and blit to screen bitmap images,
-- should be enough to construct minimalist user interfaces.
module Happlets.View.Types2D
  ( -- ** Sample Coordinate
    SampCoord, PixCoord, PixSize, sampCoord,
    -- ** Typeclasses
    Has2DOrigin(..), Is2DPrimitive(..), Quantizable(..), HasMidpoint(..),
    Canonical2D(..), ContainsPoint2D(..),
    -- ** The 'Drawing' datatype
    Drawing(..), drawing, drawingIsNull, drawingIntersects,
    drawingPrimitives, drawingCountPrimitives,
    -- ** Primitives
    Draw2DPrimitive(..), Draw2DShape(..), Map2DShape(..),
    -- *** Points
    Point2D, Size2D,
    point2D, size2D, pointX, pointY, pointXY, bounds2DPoints,
    -- *** Lines
    Line2D(..), line2D, line2DHead, line2DTail, line2DPoints,
    -- *** Rectangles
    Rect2D(..), Rect2DUnion, rect2DUnion, rect2DUnionToList, rect2DUnionCount, rect2DUnionNull,
    rect2D, rect2DSize, rect2DHead, rect2DTail, rect2DPoints,
    rect2DCenter, rect2DCentre, rect2DArea, rect2DContainsRect, bounds2DExpandLineWidth,
    rect2DMinBoundsOf, rect2DMinBoundsForAll, rect2DIntersect, rect2DDiagonal, rect2DtoInt,
    MaybeSingleton2D(..), HasBoundingBox(..),
    -- *** Arcs
    Magnitude(..), ArcRadius, Angle(..), StartAngle, EndAngle,
    Arc2D(..), arc2D, arc2DOrigin, arc2DRadius, arc2DStart, arc2DEnd,
    -- *** Paths
    Path2D, path2D, path2DOrigin, path2DPoints,
    -- *** Cubic Bezier Spline Paths
    Cubic2D, Cubic2DSegment(..), cubic2D, cubic2DOrigin, cubic2DPoints,
    cubic2DCtrlPt1, cubic2DCtrlPt2, cubic2DEndPoint,
    -- *** Matrix Transformations
    --BoundingBox2D(..), boxBounds2D, boxTransform2D, boxModel2D,
    Transform2D(..), idTrans2D, transform2D, trans2DDraw,
    -- *** Fill Types
    Draw2DFillStroke(..), fillStrokeLineWidth,
    LineWidth,
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

import           Happlets.View.Color (Color, get32BitsRGBA, set32BitsRGBA)

import           Control.Arrow ((&&&), (>>>))
import           Control.Lens
                 ( (&), (^.), (.~), (+~), (-~),
                   Lens', Iso', lens, iso, view, cloneLens
                 )
import           Control.Monad (mapM_, guard)

import           Data.Int  (Int32)
import           Data.Function (on)
import           Data.List (sortBy, nubBy)
import           Data.Maybe (isJust)
import           Data.Ord (Down(..))
import qualified Data.Vector.Unboxed  as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import           Data.Word (Word32)
import qualified Data.Vector as Vec

import           Linear.V2 (V2(..))
import           Linear.Matrix (M44, identity)

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
  map2DShape :: (Real n, UVec.Unbox n) => (n -> n) -> shape n -> shape n

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

-- | Compute the bounding box for a list of 'Point2D' values. Provide an initial point and a list of
-- points to which it is compared.
bounds2DPoints :: (Ord n, Num n) => Point2D n -> [Point2D n] -> Rect2D n
bounds2DPoints (V2 x0 y0) points = rect2D &
  rect2DTail .~ V2 minX minY &
  rect2DHead .~ V2 maxX maxY
  where
    minmax same@(lo, hi) p =
      if p > hi then (lo, p) else
      if p < lo then (p, hi) else
      same
    ((minX, maxX), (minY, maxY)) = foldl
      (\ (xs, ys) (x, y) -> (minmax xs x, minmax ys y))
      ((x0, x0), (y0, y0))
      (view pointXY <$> points)

----------------------------------------------------------------------------------------------------

-- | A class of data types that have a canonical form.
--
-- The 'Rect2D' instantiation of this re-orders the bounding 'Point2D's of the 'Rect2D' such that
-- 'rect2DHead' is the point closest to the origin @('Linear.V2.V2' 0 0)@ and the 'rect2DTail' is
-- the point furthest from the origin.
--
-- 'UnionRect2D' instantiates this class, it's canonical form in which the list representation
-- always sorts larger rectangles toward the front of the list, and smaller rectangles that are
-- fully contained with larger rectangles are removed from the set.
class Canonical2D r where { canonicalize2DShape :: r -> r; }

instance Ord n => Canonical2D (Rect2D n) where
  canonicalize2DShape (Rect2D (V2 x0 y0) (V2 x1 y1)) =
    Rect2D (V2 (min x0 x1) (min y0 y1)) (V2 (max x0 x1) (max y0 y1))

instance (Ord n, Num n, UMVec.Unbox n) => Canonical2D (Rect2DUnion n) where
  canonicalize2DShape =
    (uncurry rect2DUnion . (id &&& length)) .
    nubBy rect2DContainsRect .
    fmap snd .
    sortBy (compare `on` fst) .
    fmap ((Down . rect2DArea &&& id) . canonicalize2DShape) .
    rect2DUnionToList

----------------------------------------------------------------------------------------------------

-- | Class of 2D-shape types which have a convex hull, provides a function that tests whether a
-- 'Point2D' lies within the convex hull.
class HasBoundingBox r => ContainsPoint2D r where
  containsPoint2D :: Bounds2DMetric r ~ n => r -> Point2D n -> Bool

instance (Ord n, Num n) => ContainsPoint2D (Rect2D n) where
  containsPoint2D r0 p =
    let r = canonicalize2DShape r0
        (V2 w h) = (r ^. rect2DHead) - (r ^. rect2DTail)
        (V2 x y) = p - (r ^. rect2DTail)
    in x >= 0 && y >= 0 && x <= w && y <= h

instance (Ord n, Num n, UMVec.Unbox n) => ContainsPoint2D (Rect2DUnion n) where
  containsPoint2D u p = or $ (`containsPoint2D` p) <$> rect2DUnionToList u

----------------------------------------------------------------------------------------------------

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

-- | This data type contains a set of 'Rect2D' values stored in an unboxed vector.
--
-- For the sake of convenience, this data type instantiates 'HasBoundingBox', but if the
-- 'Rect2DUnion' is empty (i.e. the 'rect2DUnionCount' is zero) then a null (all zero) 'Rect2D'
-- value is the result of 'theBoundingBox'.
data Rect2DUnion n
  = Rect2DUnion
    { rect2DUnionBounds :: !(Rect2D n)
    , rect2DUnionVector :: !(UVec.Vector n)
    }

instance (Ord n, Num n, UMVec.Unbox n) => Semigroup (Rect2DUnion n) where
  (<>) (Rect2DUnion{rect2DUnionBounds=bndsA,rect2DUnionVector=vecA})
       (Rect2DUnion{rect2DUnionBounds=bndsB,rect2DUnionVector=vecB}) =
    Rect2DUnion
    { rect2DUnionBounds = rect2DMinBoundsOf bndsA bndsB
    , rect2DUnionVector = vecA <> vecB
    }

instance (Ord n, Num n, UMVec.Unbox n) => Monoid (Rect2DUnion n) where
  mempty = Rect2DUnion{ rect2DUnionBounds = rect2D, rect2DUnionVector = mempty }
  mappend = (<>)

-- | Returns the number of rectangular elements in a given 'Rect2DUnion'
rect2DUnionCount :: UMVec.Unbox n => Rect2DUnion n -> Int
rect2DUnionCount (Rect2DUnion{rect2DUnionVector=vec}) = UVec.length vec `div` 4

-- | True if the 'Rect2DUnion' is empty.
rect2DUnionNull :: UMVec.Unbox n => Rect2DUnion n -> Bool
rect2DUnionNull (Rect2DUnion{rect2DUnionVector=vec}) = UVec.null vec

-- | Construct a 'Rect2DUnion'. Internally, an unboxed vector is constructed, so the number of
-- elements in the union needs to be given to this constructor, also to ensure an infinite list is
-- not provided. The constructed union is not immediately canonicalized, so evaluating
-- 'rect2DUnionToList' on a 'Rect2DUnion' constructed by this function without first evaluating
-- 'canonicalize2DShape' will produce the same list that was passed to this function up to n
-- elements (where @n@ is the number of elements given as the second parameter).
rect2DUnion :: (Ord n, Num n, UVec.Unbox n) => [Rect2D n] -> Int -> Rect2DUnion n
rect2DUnion rectlist nelems =
  Rect2DUnion
  { rect2DUnionBounds = rect2DMinBoundsForAll rectlist
  , rect2DUnionVector = UVec.create
      (do vec <- UMVec.new (4 * nelems)
          mapM_ (uncurry $ UMVec.write vec) $ zip [0 ..] $ do
            r <- rectlist
            let (V2 a b) = r ^. rect2DHead
            let (V2 c d) = r ^. rect2DTail
            [a, b, c, d]
          return vec
      )
  }

rect2DUnionToList :: (Num n, UMVec.Unbox n) => Rect2DUnion n -> [Rect2D n]
rect2DUnionToList (Rect2DUnion{rect2DUnionVector=vec}) = loop $ UVec.toList vec where
  loop = \ case
    a:b:c:d:more -> (rect2D & rect2DHead .~ V2 a b & rect2DTail .~ V2 c d) : loop more
    _ -> []

----------------------------------------------------------------------------------------------------

class HasBoundingBox a where
  type Bounds2DMetric a
  theBoundingBox :: a -> Rect2D (Bounds2DMetric a)

instance HasBoundingBox (Rect2DUnion n) where
  type Bounds2DMetric (Rect2DUnion n) = n
  theBoundingBox = rect2DUnionBounds

instance HasBoundingBox (Rect2D n) where
  type Bounds2DMetric (Rect2D n) = n
  theBoundingBox = id; 

instance HasBoundingBox (Line2D n) where
  type Bounds2DMetric (Line2D n) = n
  theBoundingBox (Line2D a b) = Rect2D a b; 

--instance HasBoundingBox (BoundingBox2D trans n model) where
--  type Bounds2DMetric (BoundingBox2D trans n model) = n
--  theBoundingBox = view boxBounds2D; 

instance (Ord n, Num n, UVec.Unbox n) => HasBoundingBox (Path2D n) where
  type Bounds2DMetric (Path2D n) = n
  theBoundingBox = uncurry bounds2DPoints . path2DPoints

instance (Ord n, Num n, UVec.Unbox n) => HasBoundingBox (Cubic2D n) where
  type Bounds2DMetric (Cubic2D n) = n
  theBoundingBox =
    uncurry bounds2DPoints .
    fmap (>>= (\ (Cubic2DSegment a b c) -> [a,b,c])) .
    cubic2DPoints

instance (Ord n, Num n, Real n, Quantizable n) => HasBoundingBox (Arc2D n) where
  type Bounds2DMetric (Arc2D n) = n
  theBoundingBox = uncurry bounds2DPoints . arc2DPoints

instance
  (Ord n, Num n, Real n, UVec.Unbox n, Quantizable n) =>
  HasBoundingBox (Draw2DShape n)
  where
    type Bounds2DMetric (Draw2DShape n) = n
    theBoundingBox = \ case
      Draw2DRect  o -> theBoundingBox o
      Draw2DArc   o -> theBoundingBox o
      Draw2DPath  o -> theBoundingBox o
      Draw2DCubic o -> theBoundingBox o

instance
  (Ord n, Num n, HasMidpoint n, Real n, UVec.Unbox n, Quantizable n) =>
  HasBoundingBox (Draw2DPrimitive n)
  where
    type Bounds2DMetric (Draw2DPrimitive n) = n
    theBoundingBox = \ case
      Draw2DReset -> rect2D
      Draw2DLines width _ lines -> case theBoundingBox <$> lines of
        []     -> rect2D
        bounds ->
          bounds2DExpandLineWidth
          (rect2DMinBoundsForAll bounds)
          width
      Draw2DShapes stroke shapes -> case theBoundingBox <$> shapes of
        []     -> rect2D
        bounds ->
          bounds2DExpandLineWidth
          (rect2DMinBoundsForAll bounds)
          (fillStrokeLineWidth stroke)

instance
  (Ord n, Num n, Real n, UVec.Unbox n, Quantizable n) =>
  HasBoundingBox (Drawing n)
  where
    type Bounds2DMetric (Drawing n) = n
    theBoundingBox = drawingBoundingBox

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

instance (Ord n, Num n) => Semigroup (Rect2D n) where { (<>) = rect2DMinBoundsOf; }
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
rect2DCenter :: (Num n, HasMidpoint n) => Rect2D n -> Point2D n
rect2DCenter r = (midpoint 0 <$> rect2DSize r) + (r ^. rect2DHead)

-- | British spelling of 'rect2DCenter'.
rect2DCentre :: (Num n, HasMidpoint n) => Rect2D n -> Point2D n
rect2DCentre = rect2DCenter

-- | Compute the area of a 'Rect2D'.
rect2DArea :: (Ord n, Num n) => Rect2D n -> n
rect2DArea = canonicalize2DShape >>> \ r ->
  let (V2 w h) = (r ^. rect2DHead) - (r ^. rect2DTail) in w * h

-- | Returns 'True' if the second (right-hand) argument is a rectangle that is fully contained
-- within the first (left-hand) argument.
rect2DContainsRect :: (Ord n, Num n) => Rect2D n -> Rect2D n -> Bool
rect2DContainsRect a b =
  let f len = containsPoint2D a (b ^. cloneLens len) in
  f rect2DHead && f rect2DTail

-- | Expresses a 'Rect2D' as a tuple of 'Point2D' values.
rect2DPoints :: Iso' (Rect2D n) (Point2D n, Point2D n)
rect2DPoints = iso (\ (Rect2D a b) -> (a, b)) (\ (a,b) -> Rect2D a b)

-- | Computes the smallest possible rectangle that can contain both rectangles, no matter how far
-- apart they are.
rect2DMinBoundsOf :: (Ord n, Num n) => Rect2D n -> Rect2D n -> Rect2D n
rect2DMinBoundsOf a@(Rect2D(V2 xa ya)(V2 xb yb)) b@(Rect2D(V2 xc yc)(V2 xd yd)) =
  if a == rect2D then b else if b == rect2D then a else
  let f4 comp a b c d = comp a $ comp b $ comp c d in Rect2D
    (V2 (f4 min xa xb xc xd) (f4 min ya yb yc yd))
    (V2 (f4 max xa xb xc xd) (f4 max ya yb yc yd))

-- | This function computes the minimum bounding box for all 'Rect2D' values in a list, excluding
-- null rectangles equal to 'rect2D'. If the given list is empty, the null 'rect2D' value is
-- returned.
rect2DMinBoundsForAll :: (Ord n, Num n) => [Rect2D n] -> Rect2D n
rect2DMinBoundsForAll = \ case
  []   -> rect2D
  r:rs -> foldl rect2DMinBoundsOf r rs

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

-- | If some 'Draw2DPrimitive' value is to be drawn with a 'Draw2DFillStroke' with a non-zero
-- 'LineWidth', then grow the given bounds 'Rect2D' of the primitive by enough such that it's ideal
-- bounding box includes the line width.
bounds2DExpandLineWidth :: (Ord n, Num n, HasMidpoint n) => Rect2D n -> LineWidth n -> Rect2D n
bounds2DExpandLineWidth box0 n =
  let box = canonicalize2DShape box0 in
  let w = midpoint 0 (abs n) in
  let w2 = V2 w w in
  box & rect2DHead +~ w2 & rect2DTail -~ w2

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

-- | Returns the origin and the points used to compute 'theBoundingBox'. The list of points returned
-- will include all points that touch the minimum bounding box of the arc.
arc2DPoints :: (Real n, Quantizable n) => Arc2D n -> (Point2D n, [Point2D n])
arc2DPoints (Arc2D (V2 x y) (Magnitude r) start0 end0) = (V2 x y, points) where
  lim theta = let mod = theta / (2*pi) in 2*pi * (mod - realToFrac (floor mod :: Integer))
  start = lim $ min start end
  end   = lim $ max start end
  between a b c = a <= (c::Double) && c <= b
  points = fmap snd $
    filter ((if start0 <= end0 then id else flip) between start end . fst)
    [ (start   , quantize <$> V2 (realToFrac x + cos start) (realToFrac y + sin start))
    , (end     , quantize <$> V2 (realToFrac x + cos end  ) (realToFrac y + sin end  ))
    , (0       , V2 (x+r) y)
    , (pi/2    , V2 x (y-r))
    , (pi      , V2 (x-r) y)
    , (3*(pi/2), V2 x (y+r))
    ]

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

-- | A function used to draw a @model@ to the canvas.
data Drawing n
  = Drawing
    { drawingBoundingBox :: !(Rect2D n)
    , unwrapDrawing :: Vec.Vector (Draw2DPrimitive n)
    }
  deriving Eq

instance Show (Drawing n) where
  show d = case Vec.toList . unwrapDrawing $ d of
    [] -> "(mempty :: Drawing)"
    [p] -> "[" <> show p <> "]"
    p:px -> "[ " <> show p <> (px >>= ("\n, " <>) . show) <> "\n]"

instance (Ord n, Num n) => Semigroup (Drawing n) where
  (<>) (Drawing{drawingBoundingBox=boxA,unwrapDrawing=a})
       (Drawing{drawingBoundingBox=boxB,unwrapDrawing=b}) =
    Drawing (rect2DMinBoundsOf boxA boxB) (a <> b)

instance (Ord n, Num n) => Monoid (Drawing n) where
  mempty = Drawing{ drawingBoundingBox = rect2D, unwrapDrawing = mempty }
  mappend = (<>)

instance Map2DShape Drawing where
  map2DShape f d = Drawing
    { drawingBoundingBox = map2DShape f $ drawingBoundingBox d
    , unwrapDrawing = map2DShape f <$> unwrapDrawing d
    }

-- | Consruct a 'Drawing' 
drawing
  :: (Ord n, Num n, Real n, HasMidpoint n, Quantizable n, UMVec.Unbox n)
  => [Draw2DPrimitive n] -> Drawing n
drawing prims = Drawing
  { unwrapDrawing = Vec.fromList prims
  , drawingBoundingBox = rect2DMinBoundsForAll $ theBoundingBox <$> prims
  }

-- | Extract a list of 'Draw2DPrimitive' data structures from a 'Drawing'.
drawingPrimitives :: Drawing n -> [Draw2DPrimitive n]
drawingPrimitives = Vec.toList . unwrapDrawing

-- | 'True' if the 'Drawing' is 'mempty'.
drawingIsNull :: Drawing n -> Bool
drawingIsNull (Drawing{unwrapDrawing=vec}) = Vec.null vec

-- | Returns 'True' if the 'drawingBoundingBox' intersects with the 'rect2DUnionBounds' of a
-- 'Rect2DUnion'.
drawingIntersects :: Ord n => Drawing n -> Rect2DUnion n -> Bool
drawingIntersects d u = isJust $ drawingBoundingBox d `rect2DIntersect` rect2DUnionBounds u

-- | Count the number of 'Draw2DPrimitive's in a 'Drawing'.
drawingCountPrimitives :: Drawing n -> Int
drawingCountPrimitives (Drawing{unwrapDrawing=vec}) = Vec.length vec

----------------------------------------------------------------------------------------------------

data Draw2DShape n
  = Draw2DRect  !(Rect2D n)
  | Draw2DArc   !(Arc2D  n)
  | Draw2DPath  !(Path2D n)
  | Draw2DCubic !(Cubic2D n)
  deriving Eq

instance Map2DShape Draw2DShape where
  map2DShape f = \ case
    Draw2DRect  shape -> Draw2DRect  $ map2DShape f shape
    Draw2DArc   shape -> Draw2DArc   $ map2DShape f shape
    Draw2DPath  shape -> Draw2DPath  $ map2DShape f shape
    Draw2DCubic shape -> Draw2DCubic $ map2DShape f shape

----------------------------------------------------------------------------------------------------

data Draw2DPrimitive n
  = Draw2DReset
    -- ^ Clears all pixels in the buffer to the 'fillColor' value.
  | Draw2DLines !(LineWidth n) !(PaintSource n) [Line2D n]
    -- ^ Draws several distinct line segments, different from 'Draw2DPath' which draws several
    -- connected line segments.
  | Draw2DShapes !(Draw2DFillStroke n) [Draw2DShape n]
    -- ^ Draws several primitive 'Draw2DShape's.
  deriving Eq

instance Show (Draw2DPrimitive n) where
  show = \ case
    Draw2DReset{}  -> "Draw2DReset"
    Draw2DLines{}  -> "Draw2DLines"
    Draw2DShapes{} -> "Draw2DShapes"

instance Map2DShape Draw2DPrimitive where
  map2DShape f = \ case
    Draw2DReset                -> Draw2DReset
    Draw2DLines w paint shapes -> Draw2DLines (f w) (map2DShape f paint) (map2DShape f <$> shapes)
    Draw2DShapes  paint shapes -> Draw2DShapes      (map2DShape f paint) (map2DShape f <$> shapes)

----------------------------------------------------------------------------------------------------

class Is2DPrimitive shape where
  to2DShape :: Num n => Draw2DFillStroke n -> [shape n] -> Draw2DPrimitive n

instance Is2DPrimitive Line2D  where
  to2DShape = Draw2DLines 1 . \ case
    FillOnly     paint   -> paint
    StrokeOnly _ paint   -> paint
    FillStroke _ _ paint -> paint
    StrokeFill _ paint _ -> paint

instance Is2DPrimitive Arc2D   where { to2DShape paint = Draw2DShapes paint . fmap Draw2DArc; }
instance Is2DPrimitive Rect2D  where { to2DShape paint = Draw2DShapes paint . fmap Draw2DRect; }
instance Is2DPrimitive Path2D  where { to2DShape paint = Draw2DShapes paint . fmap Draw2DPath; }
instance Is2DPrimitive Cubic2D where { to2DShape paint = Draw2DShapes paint . fmap Draw2DCubic; }

----------------------------------------------------------------------------------------------------

-- | A number (typically of some 'Integral' type) that can be mapped from some 'RealFrac' value.
class Quantizable n where { quantize :: RealFrac r => r -> n }
instance Quantizable Double where { quantize = realToFrac; }
instance Quantizable Float where { quantize = realToFrac; }
instance Quantizable Int32 where { quantize = round; }
instance Quantizable Int where { quantize = round; }
instance Quantizable Word where { quantize = round; }
instance Quantizable n => Quantizable (Magnitude n) where
  quantize = Magnitude . quantize

----------------------------------------------------------------------------------------------------

-- | A class of values for which it makes sense to compute a "midpoint" value. The midpoint should
-- be negative if the first argument is greater than the second, positive if the first argument is
-- less than the second, or otherwise zero if the first and second arguments are equal.
class HasMidpoint n where { midpoint :: n -> n -> n; }
instance HasMidpoint Double where { midpoint = midpointFractional; }
instance HasMidpoint Float where { midpoint = midpointFractional; }
instance HasMidpoint Int where { midpoint = midpointIntegral; }
instance HasMidpoint Int32 where { midpoint = midpointIntegral; }
instance HasMidpoint n => HasMidpoint (Magnitude n) where
  midpoint (Magnitude a) (Magnitude b) = Magnitude $ midpoint a b

-- | Compute midpoint for any 'Fractional' value.
midpointFractional :: Fractional n => n -> n -> n
midpointFractional a b = (a - b) / 2

-- | Compute midpoint for any 'Integral' value.
midpointIntegral :: Integral n => n -> n -> n
midpointIntegral a b = (a - b) `div` 2

----------------------------------------------------------------------------------------------------

-- | Things that have a starting point. For circles, this is the center point, but for lines or
-- rectangles, this is the 'rect2DHead' or 'line2DHead' point and not the center point as you might
-- expect.
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
    , thePaintSourceFunction :: (PaintSourceFunction n)
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
  | GradientSource    (Transform2D n PaintGradient)
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

----------------------------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------------------------

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

-- | Values of this type are used when drawing lines or borders around rectangls.
type LineWidth n = n

-- | Most 'Draw2DPrimitive's enclose an 2D region within a path that can be filled, as well as
-- having a border line drawn along the enclosing path, as though a brush were stroking some paint
-- along the path. This function specifies the order of fill and stroke operations to perform.
data Draw2DFillStroke n
  = FillOnly   (PaintSource n)
    -- ^ Only draw the area of the shape, not the outline.
  | StrokeOnly !(LineWidth  n) (PaintSource n)
    -- ^ Only draw the outline of the shape, not the area.
  | FillStroke !(LineWidth  n) (PaintSource n) (PaintSource n)
    -- ^ Draw first the area of the shape, and then on top of that, draw the outline of the
    -- shape. The first 'PaintSource' is used for the fill (area), the second 'PaintSource' is used
    -- for the stroke (outline). The 'LineWidth' defines the thickness of the stroke.
  | StrokeFill !(LineWidth  n) (PaintSource n) (PaintSource n)
    -- ^ Draw first the outline of the shape, and then on top of that, draw the area of the
    -- shape. The first 'PaintSource' is used for the stroke (outline), the second 'PaintSource' is
    -- used for the fill (area). The 'LineWidth' defines the thickness of the stroke.
  deriving Eq

instance Map2DShape Draw2DFillStroke where
  map2DShape f = \ case
    FillOnly   a     -> FillOnly         (map2DShape f a)
    StrokeOnly w b   -> StrokeOnly (f w) (map2DShape f b)
    FillStroke w a b -> FillStroke (f w) (map2DShape f a) (map2DShape f b)
    StrokeFill w b a -> StrokeFill (f w) (map2DShape f b) (map2DShape f a)

-- | Get the 'LineWidth' of a 'Draw2DFillStroke' value. 'FillOnly' returns a zero width.
fillStrokeLineWidth :: Num n => Draw2DFillStroke n -> LineWidth n
fillStrokeLineWidth = \ case
  FillOnly{} -> 0
  StrokeOnly w _ -> w
  FillStroke w _ _ -> w
  StrokeFill w _ _ -> w
