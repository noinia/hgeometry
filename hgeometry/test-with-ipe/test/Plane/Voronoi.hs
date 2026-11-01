module Plane.Voronoi
  ( BoundedVoronoiDiagram
  , voronoiDiagramIn

  , closestAt
  , closestAt'
  ) where

import           Data.Foldable
import           Data.Bifunctor
import           Control.Lens
import           Data.Foldable1
import qualified Data.Map.Monoidal as MonoidalMap
import           Data.Map.Monoidal (MonoidalMap)
import           HGeometry.Polygon.Convex
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Plane
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.Triangle
import           Plane.BruteForce
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import qualified Data.Set as Set

--------------------------------------------------------------------------------

type BoundedVoronoiDiagram r site =
  MonoidalMap site (ConvexPolygon (Vertex' (EnvVertex r site) r site))

-- | Computes the voronoi diagram in a given region
voronoiDiagramIn        :: ( Point_ point 2 r, Ord r, Fractional r
                           , Foldable1 set, Functor set
                           , Ord point
                           , Show point, Show r -- FIXME
                           )
                        => Triangle (Point 2 r) -> set point -> BoundedVoronoiDiagram r point
voronoiDiagramIn domain = MonoidalMap.mapKeys (^.extra)
                        . fmap (over vertices (first (fmap (^.extra))))
                        . lowerEnvelopeOn domain
                        . fmap (\p -> pointToPlane p :+ p)
  -- TODO: figure out if mapping monotonically is safe...

-- | Given a site and a voronoi diagram, find the set of sites that
-- are closest at the query point i.e. find the set of cells that
-- contain the query point.
--
-- (this uses a very naive O(n) time implementation)
closestAt      :: ( Point_ queryPoint 2 r
                  , Point_ site 2 r, Ord r, Fractional r, Ord site
                  -- , Show site, Show r
                  , HasIntersectionWith queryPoint (ConvexPolygon (OriginalOrExtra (EnvVertex r site) (Point 2 r :+ r)))
                  ) => queryPoint -> BoundedVoronoiDiagram r site -> Set.Set site
closestAt q = MonoidalMap.keysSet
            . MonoidalMap.filter (\cell -> q `intersects` cell)

-- | Naive closest at implementation
closestAt'   :: ( Point_ queryPoint 2 r
                , Point_ site 2 r, Ord r, Fractional r, Ord site
                , Foldable set
                -- , Show site, Show r,
                ) => queryPoint -> set site -> Set.Set site
closestAt' q = maybe mempty fst
             . MonoidalMap.minView
             . foldMap (\s -> MonoidalMap.singleton (squaredEuclideanDist q s) (Set.singleton s)
                       )
