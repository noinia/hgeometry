--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Connected.FromIntersectingSegments
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computing a Connected Plane graph from a set of intersecting line
-- segments.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Connected.FromIntersectingSegments
  ( fromIntersectingSegments
  ) where

import Data.Foldable1
import Data.Foldable1.WithIndex
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Semialign
import Data.Sequence qualified as Seq
import Control.Lens
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.LineSegment.Intersection.BentleyOttmann
import HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import HGeometry.PlaneGraph.Connected.Type
import HGeometry.Point
import HGeometry.Ext
import HGeometry.Sequence.NonEmpty (ViewL1(..), asViewL1, singletonL1)
import HGeometry.Map.NonEmpty.Monoidal qualified as MonoidalNEMap
import Hiraffe.PlanarGraph.Connected
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (zipWith)
import Data.Coerce

--------------------------------------------------------------------------------

-- |  Construct A connected PlaneGraph from a set of intersecting segments.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
fromIntersectingSegments      :: forall s nonEmpty ix lineSegment segment r point.
                                 ( Foldable1 nonEmpty
                                 , FunctorWithIndex ix nonEmpty

                                 , LineSegment_ lineSegment point
                                 , Point_ point 2 r, Ord r, Fractional r
                                 , Intersection lineSegment lineSegment
                                   ~ Maybe (LineSegmentLineSegmentIntersection segment)
                                 , LineSegment_ segment point
                                 , IsIntersectableWith lineSegment lineSegment
                                 , Ord ix
                                 , HasOnSegment lineSegment 2
                                 , StartPointOf lineSegment ~ EndPointOf lineSegment


                                 , Eq lineSegment -- FIXME
                                 )
                              => nonEmpty lineSegment
                              -> CPlaneGraph s (Point 2 r :+ Seq.Seq point)
                                               (ViewL1 lineSegment)
                                               ()
fromIntersectingSegments segs = fromIntersections segs (intersections segs)

----------------------------------------

-- | Construct A connected PlaneGraph from a set of intersecting
-- segments. This assumes we are actually given the intersections as
-- well.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
--
-- note: this implementation uses that the lineSegment type is Orderable. Consider using
-- 'ByIndex Int segment' to order by some Identifier, rather than ordering by the raw segments.
fromIntersections             :: forall s nonEmpty lineSegment r point planeGraph.
                                   ( Foldable1 nonEmpty
                                   , LineSegment_ lineSegment point
                                   , Point_ point 2 r, Ord r, Num r
                                   , IsIntersectableWith lineSegment lineSegment
                                   , OrdArounds lineSegment
                                   -- , Ord lineSegment

                                   , Eq lineSegment -- FIXME: not sure where this is coming from

                                   , planeGraph ~ CPlaneGraph s (Point 2 r :+ Seq.Seq point)
                                                                (ViewL1 lineSegment)
                                                                ()
                                   )
                              => nonEmpty lineSegment
                              -> Intersections r lineSegment
                              -> planeGraph
fromIntersections segs inters = fromAdjacencyLists adjLists
  where
    -- | Map every Point to its vertexId
    vertexMapping :: MonoidalNEMap (Point 2 r) (VertexIx planeGraph)
    vertexMapping = coerce $ assignIndex vertexLocations

    -- | For each vertex location, collect the segment endpoints
    segmentEndPoints :: MonoidalNEMap (Point 2 r) (ViewL1 point)
    segmentEndPoints = foldMap1 (\(LineSegment_ s t) ->
                                   MonoidalNEMap.singleton (s^.asPoint) (singletonL1 s)
                                <> MonoidalNEMap.singleton (t^.asPoint) (singletonL1 t)
                                ) segs

    vertexLocations :: MonoidalNEMap (Point 2 r) (Associated lineSegment)
    vertexLocations = foldMap1 (\seg -> MonoidalNEMap.singleton (seg^.start.asPoint) (mkAroundStart seg)
                                     <> MonoidalNEMap.singleton (seg^.end.asPoint)   (mkAroundEnd seg)
                               ) segs
                    <>> inters

    -- | Compute, for each input segment its canonical line segment;
    -- i.e. if there are duplicate segments; we all represent them using the same ClosedLineSegment
    canonicalSegmentMap :: MonoidalNEMap (ClosedLineSegment (Point 2 r)) (ViewL1 lineSegment)
    canonicalSegmentMap =
      foldMap1 (\seg -> MonoidalNEMap.singleton (toKey seg) (singletonL1 seg)) segs

    toKey seg = let s = seg^.start.asPoint
                    t = seg^.end.asPoint
                in if s <= t then ClosedLineSegment s t else ClosedLineSegment t s

    -- the canonical segments themselves
    canonicalSegments = MonoidalNEMap.keysSet canonicalSegmentMap

    -- | Computes the vertices along each segment.
    --
    -- We actually represent every original input segment by a canonical ClosedLineSegment
    -- to appropriately handle duplicate input segments.
    verticesBySegment :: MonoidalNEMap (ClosedLineSegment (Point 2 r))
                                       (ViewL1 (VertexIx planeGraph))
    verticesBySegment =
        imap collect $ interiorIntersectionsBySegment toKey canonicalSegments inters
      where
        collect seg interiorPts = (vertexMapping MonoidalNEMap.!) <$>
              (seg^.start.asPoint) :<< (interiorPts' Seq.|> seg^.end.asPoint)
          where
            interiorPts' = Seq.sortOn alongSegment interiorPts
            -- | We want to sort the points, which all lie on the segment, along the segment
            -- to this end we essentially compare their distance to the starting point.
            -- however, instead of explicitly computing this distance, it suffices to compare
            -- the x-coordinates or y-coordiantes of the points themselves, depending on the
            -- orientation of the segment. (since a segment is xy-monotone)
            alongSegment :: Point 2 r -> r
            alongSegment = case (seg^.start.xCoord) `compare` (seg^.end.xCoord) of
              LT                                        -> view xCoord
              GT                                        -> negate . view xCoord
              EQ | seg^.start.yCoord <= seg^.end.yCoord -> view yCoord
                 | otherwise                            -> negate . view yCoord

    -- | For every vertex, collect its neighbours
    neighbours :: MonoidalNEMap (VertexIx planeGraph)
                                (MonoidalNEMap (VertexIx planeGraph) (ViewL1 lineSegment))
    neighbours = ifoldMap1 collect verticesBySegment
      where
        collect canonicalSeg verts@(_ :<< rest') = case asViewL1 rest' of
            Nothing   ->
              error "fromIntersections. absurd. every seg should have at least 2 vertices"
            Just rest -> fold1 $ zipWith f verts rest
          where
            -- This canonical segment represents a bunch of actual segments, associate
            -- the vertices with those line segments (rather than the canonical one)
            theSegs = canonicalSegmentMap MonoidalNEMap.! canonicalSeg

            f u v = MonoidalNEMap.singleton u (MonoidalNEMap.singleton v theSegs)
                 <> MonoidalNEMap.singleton v (MonoidalNEMap.singleton u theSegs)

    -- I think this already automatically takes care of colinear semgents as well, as we
    -- are using a NESet to collect the neighbours of each vertex.

    -- | Construct the final adjacency lists
    adjLists :: MonoidalNEMap _
                  (VertexIx planeGraph, Point 2 r :+ Seq.Seq point
                  , NonEmpty ( VertexIx planeGraph
                             , ViewL1 lineSegment
                             )
                  )
    adjLists = imap buildVertex vertexMapping
    buildVertex v vi = (vi, v :+ segEndpts, MonoidalNEMap.assocs $ neighbours MonoidalNEMap.! vi)
      where
        segEndpts = case segmentEndPoints MonoidalNEMap.!? v of
                      Nothing -> mempty
                      Just (x :<< rest) -> x Seq.<| rest

-- | Computes the interior intersections on each segment.
--
-- O((n+k)\log n)
interiorIntersectionsBySegment                   :: ( Ord lineSegment
                                                    , Foldable1 nonEmpty)
                                                 => (richSegment -> lineSegment)
                                                 -> nonEmpty lineSegment
                                                    -- ^ all input segments
                                                 -> Intersections r richSegment
                                                 -> MonoidalNEMap lineSegment (Seq.Seq (Point 2 r))
interiorIntersectionsBySegment toKey segs inters =
        foldMap1 (flip MonoidalNEMap.singleton mempty) segs
    <>> coerce (ifoldMap construct inters)
  where
    construct p assoc =
      foldMap (\seg -> MonoidalMap.singleton (toKey $ coerce seg) (Seq.singleton p))
              (assoc^.interiorTo)


-- | Assign each element in the map a unique Integer key (in the range \([0,n)\) )
assignIndex :: MonoidalNEMap k v -> MonoidalNEMap k Int
assignIndex = snd . MonoidalNEMap.mapAccumWithKey (\i _ _ -> (succ i, i)) 0


-- | Helper to combine a nonempty monoidal map with an additional map
(<>>)        :: (Ord k, Semigroup v) => MonoidalNEMap k v -> Map.Map k v -> MonoidalNEMap k v
base <>> new = foldr (uncurry MonoidalNEMap.insert) base $ Map.toAscList new
