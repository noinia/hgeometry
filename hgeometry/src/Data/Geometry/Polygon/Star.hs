module Data.Geometry.Polygon.Star where

import Control.Monad.Random
import Data.Ratio

import Data.Geometry.Polygon.Core
import Data.Geometry.Polygon.Convex
import Data.Geometry.Point


{- Generating random star-shaped polygons.

Star shaped polygons have an area somewhere inside of the polygon
from where the entire polygon is visible.

We can generate a random star-shaped polygon by first generating a
random set of points, computing the convex hull and picking a random
point inside of it, then finally sorting the set of points counter-clockwise
around the select center-point.

Why do we want random star-shaped polygons? They make it a lot easier to
test and benchmark algorithms.

I've copied in a helper function for generating random points. It
came from hgeometry-showcase but I'll eventually put it in
Data.Geometry.Point.

Other functions you should use:
  Data.Ext
    ext :: a -> a :+ ()
  Data.Geometry.Point
    sortAround :: (Ord r, Num r) => Point 2 r -> [Point 2 r] -> [Point 2 r]
  Algorithms.Geometry.ConvexHull.GrahamScan
    convexHull :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
  Data.Geometry.Polygon.Core
    pickPoint  :: (Ord r, Fractional r) => Polygon p t r -> Point 2 r
    simpleFromPoints :: forall p r. (Ord r, Fractional r) => [Point 2 r :+ p] -> SimplePolygon p r

So, to recap, this is the algorithm
 1. Generate N random points, let's call them S
 2. Compute the convex hull of S
 3. Pick a point inside of the convex hull. It must not be outside or on the boundary.
 4. Sort S around the point we picked in 3.
 5. Create a polygon from the sorted points.

If you have any questions, don't hesitate to reach out to me.
Good luck and have fun,
David.

Bonus objective:
  'pickPoint' from Data.Geometry.Polygon.Core does not select a random point.
  Selecting a random point from an arbitrary polygon is a bit tricky. However,
  it is much easier with a convex polygon. Convex polygons can be easily
  broken into a list of triangles. Then a random point can be drawn from the
  triangles, with frequencies set by the size of each triangle.

  If you're up for it, it would be amazing if you wrote the code for picking
  a random point inside of a convex polygon. It's quite a bit more difficult
  than just implementing 'randomStar', though.

-}
randomStar :: RandomGen g => Int -> Rand g (SimplePolygon () Rational)
randomStar nVertices = error "not yet written"

randomPointConvex :: RandomGen g => ConvexPolygon p Rational -> Rand g (Point 2 Rational)
randomPointConvex = error "bonus objective - finish randomStar first"

-------------------------------------------------------------------------------
-- Helper functions:

granularity :: Integer
granularity = 10000000

maxX, maxY :: Rational
maxX = 9
maxY = 9

-- Random point between screenTop/screenBottom.
genPoint :: RandomGen g => Rand g (Point 2 Rational)
genPoint = do
  x <- liftRand $ randomR (0, granularity)
  y <- liftRand $ randomR (0, granularity)
  pure $ Point2
    ((x % granularity) * maxX - maxX/2)
    ((y % granularity) * maxY - maxY/2)

genPoints :: RandomGen g => Int -> Rand g [Point 2 Rational]
genPoints n = replicateM n genPoint


