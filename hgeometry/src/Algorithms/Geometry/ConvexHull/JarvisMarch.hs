module Algorithms.Geometry.ConvexHull.JarvisMarch(convexHull) where

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex (ConvexPolygon(..))
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util

import Data.Ratio

--------------------------------------------------------------------------------

-- | Compute the convexhull using JarvisMarch. The resulting polygon
-- is given in clockwise order.
--
-- running time: \(O(nh)\), where \(n\) is the number of input points
-- and \(h\) is the complexity of the hull.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon . fromPoints $ [p]
convexHull ps        = ConvexPolygon . fromPoints . takeWhile' (\(q :+ _) -> c /= q)
                     $ iterate' next p (NonEmpty.fromList pts)
                       -- note that fromList is afe since ps contains at least two elements
  where
    SP p@(c :+ _) pts = minViewBy incXdecY ps
    takeWhile' pf (x :| xs) = x : takeWhile pf xs

-- | Computes an infinite list using the given functio and the starting values.
iterate'   :: (a -> NonEmpty a -> SP a (NonEmpty a)) -> a -> NonEmpty a -> NonEmpty a
iterate' f = go
  where
    go p xs = let SP q qs = f p xs
              in p <| go q qs

-- | Computes the next point on the hull
next       :: (Ord r, Num r) => Point 2 r :+ p -> NonEmpty (Point 2 r :+ p)
           -> SP (Point 2 r :+ p) (NonEmpty (Point 2 r :+ p))
next p pts = let SP q qts = maxViewBy (ccwCmpAround p) pts
             in SP q (p :| qts)

  --FIXME: we should order w.r.t. the previous edge of the CH, not wrt
  --the positive x-axis (which is what ccwCmparound does by default).

-- | Extracts the minimum value
minViewBy     :: (a -> a -> Ordering) -> NonEmpty a -> SP a [a]
minViewBy cmp = maxViewBy (flip cmp)

-- | Extracts the maximum value, accoridng to the given ordering, from
-- the NonEmpty.
--
-- \(O(n)\)
maxViewBy               :: (a -> a -> Ordering) -> NonEmpty a -> SP a [a]
maxViewBy cmp (y :| ys) = foldr f (SP y []) ys
  where
    f x (SP m xs) = case x `cmp` m of
                      LT -> SP m (x:xs)
                      EQ -> SP m (x:xs)
                      GT -> SP x (m:xs)

incXdecY  :: Ord r => (Point 2 r) :+ p -> (Point 2 r) :+ q -> Ordering
incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
  compare px qx <> compare qy py

mPoint2 [x,y] = Point2 x y

testPoints = NonEmpty.fromList
  [ mPoint2 [4328144040405 % 9191237772517,(-656816939883) % 366010053794] :+ 1
  , mPoint2 [5431993630863 % 5644338691895,1429203164490 % 3260120057419] :+ 2
  , mPoint2 [(-1790656476600) % 1442188070887,(-3336687564687) % 2544102927449] :+ 3
  ]

toDouble          :: Point 2 Rational :+ a -> Point 2 Double :+ a
toDouble (p :+ x) = (realToFrac <$> p) :+ x
