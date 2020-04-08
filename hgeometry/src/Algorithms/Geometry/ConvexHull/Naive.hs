module Algorithms.Geometry.ConvexHull.Naive( ConvexHull
                                           , lowerHull', lowerHullAll

                                           , isValidTriangle, upperHalfSpaceOf
                                           ) where

import           Control.Lens
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.HalfSpace
import           Data.Geometry.HyperPlane
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import           Data.Intersection(intersects)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (listToMaybe, isNothing)
import           Data.Util
--------------------------------------------------------------------------------

type ConvexHull d p r = [Triangle 3 p r]

-- | Computes the lower hull without its vertical triangles.
--
-- pre: The points are in general position. In particular, no four
-- points should be coplanar.
--
-- running time: \(O(n^4)\)
lowerHull' :: forall r p. (Ord r, Fractional r, Show r)
           => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' = filter (not . isVertical) . lowerHullAll
  where
    isVertical (Triangle p q r) =
      ccw' (p&core %~ projectPoint) (q&core %~ projectPoint) (r&core %~ projectPoint) == CoLinear

-- | Generates a set of triangles to be used to construct a complete
-- convex hull. In particular, it may contain vertical triangles.
--
-- pre: The points are in general position. In particular, no four
-- points should be coplanar.
--
-- running time: \(O(n^4)\)
lowerHullAll                 :: forall r p. (Ord r, Fractional r, Show r)
                             => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHullAll (toList -> pts) = let mkT (Three p q r) = Triangle p q r in
    [ t | t <- mkT <$> uniqueTriplets pts, isNothing (isValidTriangle t pts) ]



killOverlapping :: (Ord r, Fractional r) => [Triangle 3 p r] -> [Triangle 3 p r]
killOverlapping = foldr keepIfNotOverlaps []
  where
    keepIfNotOverlaps t ts | any (t `overlaps`) ts = ts
                           | otherwise             = t:ts


t1@(Triangle p q r) `overlaps` t2@(Triangle a b c) = upperHalfSpaceOf t1 == upperHalfSpaceOf t2
                                                  && False



-- | Tests if this is a valid triangle for the lower envelope. That
-- is, if all point lie above the plane through these points. Returns
-- a Maybe; if the result is a Nothing the triangle is valid, if not
-- it returns a counter example.
--
-- >>> let t = (Triangle (ext origin) (ext $ Point3 1 0 0) (ext $ Point3 0 1 0))
-- >>> isValidTriangle t [ext $ Point3 5 5 0]
-- Nothing
-- >>> let t = (Triangle (ext origin) (ext $ Point3 1 0 0) (ext $ Point3 0 1 0))
-- >>> isValidTriangle t [ext $ Point3 5 5 (-10)]
-- Just (Point3 [5,5,-10] :+ ())
isValidTriangle   :: (Num r, Ord r)
                  => Triangle 3 p r -> [Point 3 r :+ q] -> Maybe (Point 3 r :+ q)
isValidTriangle t = listToMaybe . filter (\a -> not $ (a^.core) `intersects` h)
  where
    h = upperHalfSpaceOf t


-- | Computes the halfspace above the triangle.
--
-- >>> upperHalfSpaceOf (Triangle (ext $ origin) (ext $ Point3 10 0 0) (ext $ Point3 0 10 0))
-- HalfSpace {_boundingPlane = HyperPlane {_inPlane = Point3 [0,0,0], _normalVec = Vector3 [0,0,100]}}
upperHalfSpaceOf                  :: (Ord r, Num r) => Triangle 3 p r -> HalfSpace 3 r
upperHalfSpaceOf (Triangle p q r) = HalfSpace h
  where
    h' = from3Points (p^.core) (q^.core) (r^.core)
    c  = p&core.zCoord -~ 1
    h  = if (c^.core) `liesBelow` h' then h' else h'&normalVec %~ ((-1) *^)
    a `liesBelow` plane = (a `onSideUpDown` plane) == Below
