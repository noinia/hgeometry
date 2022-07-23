--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Inflate
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--------------------------------------------------------------------------------
module Geometry.Polygon.Inflate
  ( Arc(..)
  , inflate
  ) where

import           Algorithms.Geometry.SSSP (SSSP, sssp, triangulate)
import           Control.Lens
import           Data.Ext
import           Data.Intersection          (IsIntersectableWith (intersect),
                                             NoIntersection (NoIntersection))
import           Data.Maybe (catMaybes)
import qualified Data.Vector as V
import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Unboxed as VU
import           Data.Vinyl (Rec (RNil, (:&)))
import           Data.Vinyl.CoRec (Handler (H), match)
import           Geometry.Line (Line, lineThrough)
import           Geometry.LineSegment  (LineSegment (LineSegment, OpenLineSegment),
                                             interpolate, sqSegmentLength)
import           Geometry.Point
import           Geometry.Polygon.Core

----------------------------------------------------
-- Implementation

-- | Points annotated with an 'Arc' indicate that the edge from this point to
--   the next should not be a straight line but instead an arc with a given center
--   and a given border edge.
data Arc r = Arc
  { arcCenter :: Point 2 r
  , arcEdge   :: (Point 2 r, Point 2 r)
  } deriving (Show)

type Parent = Int

markParents :: SSSP -> SimplePolygon p r -> SimplePolygon Parent r
markParents t p = unsafeFromCircularVector $
  CV.imap (\i (pt :+ _) -> pt :+ t VU.! i) (p^.outerBoundaryVector)

addSteinerPoints :: forall r. (Ord r, Fractional r) => SimplePolygon Parent r -> SimplePolygon Parent r
addSteinerPoints p = fromPoints $ concatMap worker [0 .. size p - 1]
  where
    worker nth = do
        pointA : catMaybes [ (:+ parent nth)     <$> getIntersection edge lineA
                           , (:+ parent (nth+1)) <$> getIntersection edge lineB ]
      where
        fetch idx = p ^. outerVertex idx
        pointA = fetch nth
        pointB = fetch (nth+1)
        parent idx = p^.outerVertex idx.extra
        lineA = lineThrough @Line
          (fetch (parent nth) ^. core)
          (fetch (parent (parent nth)) ^. core)
        lineB = lineThrough @Line
          (fetch (parent (nth+1)) ^. core)
          (fetch (parent (parent (nth+1))) ^. core)
        edge = OpenLineSegment pointA pointB
        getIntersection              :: LineSegment 2 p r -> Line 2 r -> Maybe (Point 2 r)
        getIntersection segment line =
          match (segment `intersect` line) (
               H (\NoIntersection -> Nothing)
            :& H (\pt -> Just pt)
            :& H (\LineSegment{} -> Nothing)
            :& RNil
          )

annotate :: (Real r, Fractional r) =>
  Double -> SimplePolygon Parent r -> SimplePolygon Parent r -> SimplePolygon (Arc r) r
annotate t original p = unsafeFromCircularVector $
    CV.imap ann (p^.outerBoundaryVector)
    -- CV.generate (size p) ann -- Use this when circular-vector-0.1.2 is out.
  where
    nO = size original
    visibleDist = V.maximum distanceTreeSum * t
    parent idx = p^.outerVertex idx.extra
    parentO idx = original^.outerVertex idx.extra
    getLineO idx = OpenLineSegment (original ^. outerVertex (parentO idx)) (original ^. outerVertex idx)
    getLineP idx = OpenLineSegment (original ^. outerVertex (parent idx)) (p ^. outerVertex idx)

    ann i _ =
        ptLocation i :+ arc
      where
        start = p ^. outerVertex i . core
        end = p ^. outerVertex (i+1) . core
        arc = Arc
          { arcCenter =
              original ^. outerVertex (commonParent original (parent i) (parent (i+1))) . core
          , arcEdge   = (start, end) }

    -- Array of locations for points in the original polygon.
    ptLocationsO = V.generate nO ptLocationO
    ptLocationO 0 = (original ^. outerVertex 0 . core)
    ptLocationO i
      | frac <= 0 = ptLocationsO V.! (parentO i)
      | frac >= 1 = (original ^. outerVertex i . core)
      | otherwise = (interpolate frac (getLineO i))
      where
        dParent = distanceTreeSum V.! parentO i
        dSelf   = oDistance VU.! i
        frac    = realToFrac ((visibleDist - dParent) / dSelf)

    -- Locations for original points and steiner points.
    ptLocation 0 = (p ^. outerVertex 0 . core)
    ptLocation i
      | frac <= 0 = ptLocationsO V.! (parent i)
      | frac >= 1 = (p ^. outerVertex i . core)
      | otherwise = (interpolate frac (getLineP i))
      where
        dParent = distanceTreeSum V.! parent i
        dSelf   = sqrt $ realToFrac $ sqSegmentLength $ getLineP i
        frac    = realToFrac ((visibleDist - dParent) / dSelf)

    oDistance = VU.generate nO $ \i ->
      case i of
        0 -> 0
        _ -> sqrt $ realToFrac $ sqSegmentLength $ getLineO i
    distanceTreeSum = V.generate nO $ \i ->
      case i of
        0 -> 0
        _ -> distanceTreeSum V.! parentO i + oDistance VU.! i

commonParent :: SimplePolygon Parent r -> Int -> Int -> Int
commonParent p a b = worker 0 (parents a) (parents b)
  where
    worker _shared (x:xs) (y:ys)
      | x == y = worker x xs ys
    worker shared _ _ = shared
    parents 0 = [0]
    parents i = parents (p ^. outerVertex i . extra) ++ [i]

-- | \( O(n \log n) \)
inflate :: (Real r, Fractional r) => Double -> SimplePolygon () r -> SimplePolygon (Arc r) r
inflate t p = annotate t marked steiner
  where
    marked = markParents (sssp (triangulate p)) p
    steiner = addSteinerPoints marked
