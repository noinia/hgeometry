module Data.Geometry.Polygon.Inflate where

import Data.Ext
import Algorithms.Geometry.SSSP
import Control.Lens
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Polygon.Core
import Data.Intersection
import Data.Maybe
import Data.Vinyl
import Data.Vinyl.CoRec

import qualified Data.Vector.Unboxed as VU

data InflatedPoint r
  = None
  | Partial (Point 2 r)
  | Done

addPseudoPoints_ :: (Ord r, Fractional r, Monoid p) => SimplePolygon p r -> SimplePolygon p r
addPseudoPoints_ p = addPseudoPoints t p
  where
    t = sssp (triangulate p)

addPseudoPoints :: (Ord r, Fractional r, Monoid p) => SSSP -> SimplePolygon p r -> SimplePolygon p r
addPseudoPoints tree p = fromPoints $ concatMap worker [0 .. size p-1]
  where
    worker nth =
        pointA : mapMaybe (getIntersection edge) [lineA,lineB]
      where
        lookup idx = p ^. outerVertex idx
        pointA = lookup nth
        pointB = lookup (nth+1)
        parent idx = tree VU.! (idx `mod` size p)
        lineA = lineThrough
          (lookup (parent nth) ^. core)
          (lookup (parent (parent nth)) ^. core)
        lineB = lineThrough
          (lookup (parent (nth+1)) ^. core)
          (lookup (parent (parent (nth+1))) ^. core)
        edge = OpenLineSegment pointA pointB
        getIntersection segment line =
          match (segment `intersect` line) (
               H (\NoIntersection -> Nothing)
            :& H (\pt -> Just (pt :+ mempty))
            :& H (\LineSegment{} -> Nothing)
            :& RNil
          )
