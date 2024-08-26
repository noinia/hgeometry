{-# LANGUAGE PartialTypeSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Visibility
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computing Visibility
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Visibility
  ( visiblePointsFrom

  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import           HGeometry.Algorithms.DivideAndConquer (mergeSortedListsBy)
import           HGeometry.Foldable.Sort
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.LineSegment (LineSegment_, start, end
                                       , LineLineSegmentIntersection(..)
                                       , HalfLineLineSegmentIntersection(..)
                                       )
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Simple.Implementation
import           HGeometry.Polygon.Simple.InPolygon
import           HGeometry.Polygon.Simple.Type
import qualified HGeometry.Set.Util as Set
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import           Witherable

--------------------------------------------------------------------------------

-- visibilityGraph :: polygon ->

-- | O((n+m)\log (n+m))
--
-- pre; the obstacle edges are pairewise disjoint (except for possibly at endpoints)
visiblePointsFrom :: forall point r queryPoint obstacleEdge endPoint f g.
                     ( Point_ point 2 r, Point_ endPoint 2 r
                     , Point_ queryPoint 2 r
                     , Ord r, Num r
                     , LineSegment_ obstacleEdge endPoint
                     , HasSupportingLine obstacleEdge
                     , Foldable f, Witherable f, Foldable g
                     , LinePV 2 r `IsIntersectableWith` obstacleEdge
                     , Intersection (LinePV 2 r) obstacleEdge ~
                       Maybe (LineLineSegmentIntersection obstacleEdge)
                     , IsIntersectableWith (HalfLine (Point 2 r)) obstacleEdge
                     , Intersection (HalfLine (Point 2 r)) obstacleEdge
                       ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r) obstacleEdge)
                     )
                  => point -> f obstacleEdge -> g queryPoint -> [queryPoint]
visiblePointsFrom p' obstacleEdges queryPoints = case F.toList queryPoints of
  []       -> []
  (q0 : _) -> snd $ foldr (handle p) (initial,[]) events -- run the sweep
    where
      p   = p'^.asPoint :: Point 2 r
      v   = (q0^.asPoint) .-. p
      ray = HalfLine p v

      -- sort the events cyclically around p
      events = mergeSortedListsBy eventCmp queryEvents endPointEvents

      queryEvents    = sortEvents $ foldMap toEvent  queryPoints
      endPointEvents = sortEvents $ foldMap toEvents obstacleEdges

      sortEvents = F.toList . sortBy @Vector eventCmp . Builder.build @Vector

      eventCmp e1 e2 = let a1 = eventPoint e1
                           a2 = eventPoint e2
                       in ccwCmpAroundWith v p a1 a2 <> cmpByDistanceTo  p a1 a2

      initial = Set.fromDistinctAscList . map snd . F.toList
              . sortBy @Vector (cmpToRay ray)
              $ mapMaybe (\s -> (,s) <$> ray `intersect` s) obstacleEdges


cmpToRay :: HalfLine (Point 2 r)
         -> (HalfLineLineSegmentIntersection (Point 2 r) edge, edge )
         -> (HalfLineLineSegmentIntersection (Point 2 r) edge, edge )
         -> Ordering
cmpToRay ray@(HalfLine p _) (x1,_) (x2,_) = undefined



-- | Handle an event in the sweep line algorithm
handle                                    :: ( HasSupportingLine obstacleEdge
                                             , LineSegment_ obstacleEdge point
                                             , Point_ point 2 r, Ord r, Num r
                                             , LinePV 2 r `IsIntersectableWith` obstacleEdge
                                             , Intersection (LinePV 2 r) obstacleEdge ~
                                               Maybe (LineLineSegmentIntersection obstacleEdge)
                                             )
                                          => Point 2 r
                                          -> Event r queryPoint obstacleEdge
                                          -> (Set.Set obstacleEdge, [queryPoint])
                                          -> (Set.Set obstacleEdge, [queryPoint])
handle p evt acc@(statusStructure,output) = case evt of
    Query q' q
      | isVisible q' -> (statusStructure, q : output)
      | otherwise    -> acc
    EndPoint q' seg  -> let cmp = cmpSegs (LinePV q' (q' .-. p))
                        in (Set.toggleBy cmp seg statusStructure, output)
  where
    -- to test if q' is visible we find the closest segment in the status structre
    -- and test if p and q' lie on the same side of this segment.
    isVisible q' = case supportingLine <$> Set.lookupMin statusStructure of
      Nothing -> True
      Just l  -> case onSideTest q' l of
        EQ -> True -- if q' happens to lie *on* this line it is also visible.
        s  -> s == onSideTest p l

--------------------------------------------------------------------------------
-- * Events

data Event r queryPoint seg = Query    !(Point 2 r) queryPoint
                            | EndPoint !(Point 2 r) seg
                            deriving (Show)

-- | Project out the point corresponding to the event
eventPoint :: Event r queryPoint seg -> Point 2 r
eventPoint = \case
  Query    p _ -> p
  EndPoint p _ -> p

-- | Turn a query point into an event
toEvent      :: Point_ queryPoint 2 r => queryPoint -> Builder.Builder (Event r queryPoint seg)
toEvent q    = Builder.singleton $ Query (q^.asPoint) q

-- | Turn a segment into two events (correspoinding to its endpoints)
toEvents      :: (LineSegment_ lineSegment endPoint, Point_ endPoint 2 r)
              => lineSegment -> Builder.Builder (Event r queryPoint lineSegment)
toEvents seg = Builder.singleton (EndPoint (seg^.start.asPoint) seg)
            <> Builder.singleton (EndPoint (seg^.end.asPoint)   seg)

--------------------------------------------------------------------------------
-- * Comparing Segments with respect to a given sweep ray

-- | Compare the the two obstacle edges by their distance along the line to p (the origin
-- of the line.)
cmpSegs                      :: ( LinePV 2 r `IsIntersectableWith` obstacleEdge
                                , Intersection (LinePV 2 r) obstacleEdge ~
                                  Maybe (LineLineSegmentIntersection obstacleEdge)
                                , LineSegment_ obstacleEdge point
                                , Point_ point 2 r, Ord r, Num r
                                )
                             => LinePV 2 r -- ^ treat the line as a ray actually!
                             -> obstacleEdge -> obstacleEdge -> Ordering
cmpSegs l@(LinePV _ _) e1 e2 = cmpIntersection l (f $ l `intersect` e1) (f $ l `intersect` e2)
  where
    f = fromMaybe (error "cmpSegs: precondition failed, no intersection")

-- | Compares the intersection points by distance to p
cmpIntersection                      :: ( LineSegment_ obstacleEdge point
                                        , Point_ point 2 r, Ord r, Num r
                                        )
                                     => LinePV 2 r
                                     -> LineLineSegmentIntersection obstacleEdge
                                     -> LineLineSegmentIntersection obstacleEdge
                                     -> Ordering
cmpIntersection l@(LinePV p _) x1 x2 = cmpByDistanceTo p (f x1) (f x2)
  -- FIXME: what do we do at equality; we should then essentially try again at an epsilon
  -- angle later. (or earlier)
  where
    f = \case
      Line_x_LineSegment_Point q         -> q
      Line_x_LineSegment_LineSegment seg -> min' (seg^.start.asPoint) (seg^.end.asPoint)

    min' a b = case cmpByDistanceTo p a b of
                 GT -> b
                 _  -> a
