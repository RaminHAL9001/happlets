-- | This module provides a bare minimum of 2D drawing primitives for drawing text, lines, and
-- rectangles, along with setting foreground and background colors, and fill and stroke colors. Also
-- provided are functions for determining if points (for example, mouse cursor locations) lie within
-- a rectangular region. These primitives, along with the functionality in the
-- 'Happlets.GUI.CanBufferImages' type class to load from disk and blit to screen bitmap images,
-- should be enough to construct minimalist user interfaces.
module Happlets.Draw.Types2D
  ( RealApprox(..), realApprox,
    Point2D, Size2D, point2D, size2D, pointX, pointY, pointXY,
    Line2D(..), line2D, line2DHead, line2DTail, line2DPoints,
    Rect2D(..), rect2D, rect2DHead, rect2DTail, pointInRect2D, rect2DPoints,
    canonicalRect2D, rect2DMinBoundOf, rect2DIntersect, rect2DDiagonal,
    MaybeSingleton2D(..), HasBoundingBox(..),
    LineWidth,
    module Happlets.Draw.SampCoord,
  ) where

import           Happlets.Draw.SampCoord

import           Control.Arrow
import           Control.Lens
import           Control.Monad

import           Data.Semigroup

import           Linear.V2

----------------------------------------------------------------------------------------------------

-- | This type represents an approximation of a 'Prelude.Real' number.
newtype RealApprox = RealApprox { unwrapRealApprox :: Double }
  deriving (Eq, Ord, Enum, Num, Real, Fractional, Floating, RealFrac)

instance Show RealApprox where { show = show . unwrapRealApprox; }

instance Read RealApprox where
  readsPrec p str = readsPrec p str >>= \ (n, rem) -> [(RealApprox n, rem)]

-- | This type represents a single point.
type Point2D n = V2 n

type Size2D n = V2 n

-- | This type represents a line segment consisting of two points.
data Line2D n = Line2D !(Point2D n) !(Point2D n)
  deriving (Eq, Functor)

-- | This type represents a rectangle bounded by two where all sides of the rectangle points are
-- parallel to the orthogonal basis vectors of the drawing plane. This data type instantiates
-- 'Data.Semigroup.Semigroup' such that the smallest possible rectangle that can contain both
-- rectangles, no matter how far apart they are, is returned.
data Rect2D n = Rect2D !(Point2D n) !(Point2D n)
  deriving (Eq, Functor)

instance Ord n => Semigroup (Rect2D n) where { (<>) = rect2DMinBoundOf; }

-- | An initializing 'Point2D' where 'pointX' and 'pointY' are both zero.
point2D :: Num n => Point2D n
point2D = V2 0 0

-- | A synonym for 'point2D'.
size2D :: Num n => Size2D n
size2D = point2D

realApprox :: Iso' RealApprox Double
realApprox = iso unwrapRealApprox RealApprox

-- | The X coordinate of a 'Point2D', a value expressing some distance along the horizontal axis.
pointX :: Lens' (Point2D n) n
pointX = lens (\ (V2 x _) -> x) $ \ (V2 _ y) x -> (V2 x y)

-- | The Y coordinate of a 'Point2D', a value expressing some distance along the horizontal axis.
pointY :: Lens' (Point2D n) n
pointY = lens (\ (V2 _ y) -> y) $ \ (V2 x _) y -> (V2 x y)

-- | Expresses the point as a tuple of @(x, y)@ coordinates
pointXY :: Iso' (Point2D n) (n, n)
pointXY = iso (\ (V2 x y) -> (x, y)) (\ (x,y) -> V2 x y)

-- | An initializing 'Line2D' where 'lineHead' and 'lineTail' are both the zero 'point2D'.
line2D :: Num n => Line2D n
line2D = Line2D point2D point2D

-- | The point at which drawing of a line segment begins.
line2DHead :: Lens' (Line2D n) (Point2D n)
line2DHead = lens (\ (Line2D a _) -> a) $ \ (Line2D _ b) a -> Line2D a b

-- | The point at which drawing of a line segment ends.
line2DTail :: Lens' (Line2D n) (Point2D n)
line2DTail = lens (\ (Line2D _ b) -> b) $ \ (Line2D a _) b -> Line2D a b

-- | Expresses a 'Line2D' as a tuple of 'Point2D' values.
line2DPoints :: Iso' (Line2D n) (Point2D n, Point2D n)
line2DPoints = iso (\ (Line2D a b) -> (a, b)) (\ (a,b) -> Line2D a b)

-- | An initializing 'Line2D' where the 'rectLower' and 'rectUpper' values are the zero 'point2D'.
rect2D :: Num n => Rect2D n
rect2D = Rect2D point2D point2D

-- | The point at which drawing of a bounding rectangle begins.
rect2DHead :: Lens' (Rect2D n) (Point2D n)
rect2DHead = lens (\ (Rect2D a _) -> a) $ \ (Rect2D a _) b -> Rect2D a b

-- | The point at which drawing of a bounding rectangle ends.
rect2DTail :: Lens' (Rect2D n) (Point2D n)
rect2DTail = lens (\ (Rect2D _ b) -> b) $ \ (Rect2D _ b) a -> Rect2D a b

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

class HasBoundingBox a where { theBoundingBox :: a n -> Rect2D n; }

instance HasBoundingBox Rect2D where { theBoundingBox = id; }

instance HasBoundingBox Line2D where { theBoundingBox (Line2D a b) = Rect2D a b; }

----------------------------------------------------------------------------------------------------

-- | Values of this type are used when drawing lines or borders around rectangls.
type LineWidth = RealApprox
