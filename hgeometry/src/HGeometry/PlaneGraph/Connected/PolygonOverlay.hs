--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Connected.PolygonOverlay
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes the Overlay of a bunch of simple polygon
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Connected.PolygonOverlay
  ( polygonOverlay
  , module HGeometry.PlaneGraph.Connected.PolygonOverlay.Types
  ) where

import Data.Foldable1
import Data.Sequence qualified as Seq
import Control.Lens
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Polygon
import HGeometry.Ext
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (zipWith)
import HGeometry.PlaneGraph.Connected.PolygonOverlay.Types

--------------------------------------------------------------------------------

-- | Given a set of polygons, constructs a plane graph whose vertices,
-- edges, and faces are tagged with the polygons that cover that face.
--
-- The current running time is \(O( (N+k) + \log (N+k) + (N+k)*m )\),
-- where \(N\) is the total complexity of all polygons, \(k\) is the
-- number of intersections between the polygons, and \(m\) is the
-- maximum complexity of a single polygon.
--
-- pre: everything forms a connected graph
polygonOverlay          :: forall nonEmpty s simplePolygon vertex r.
                           ( Foldable1 nonEmpty, Point_ vertex 2 r
                           , SimplePolygon_ simplePolygon vertex r
                           , Fractional r, Ord r
                           , Ord vertex
                           , HasIntersectionWith (Point 2 r) simplePolygon

                           -- , Show r, Show vertex, Show simplePolygon
                           , Eq simplePolygon -- FIXME
                           )
                        => nonEmpty simplePolygon
                        -> CPlaneGraph s (V simplePolygon)
                                         (E simplePolygon)
                                         (F simplePolygon)
polygonOverlay polygons = gr2&vertices %~ \(p :+ defs) -> V p defs (polygonsCoveringVertices p)
  where
    segs :: NonEmpty (ClosedLineSegment vertex :+ simplePolygon)
    segs = foldMap1 (\poly -> (:+ poly) <$> toNonEmptyOf outerBoundaryEdgeSegments poly
                    ) polygons

    gr  = fromIntersectingSegments segs

    gr1 :: CPlaneGraph s (Point 2 r :+ Seq.Seq vertex) (E simplePolygon) ()
    gr1 = gr&edges %@~ polygonsCoveringEdges

    gr2 :: CPlaneGraph s (Point 2 r :+ Seq.Seq vertex)
                         (E simplePolygon)
                         (F simplePolygon)
    gr2 = gr1&faces .@~ polygonsCoveringFaces

    outerId = outerFaceId gr

    polygonsCoveringFaces fi
      | fi == outerId = F mempty Nothing -- its impossible for polygons to cover the outerface
      | otherwise     = let p        = pointIn fi
                            covering = filter' (p `intersects`) polygons
                        in F covering (Just p)

    filter' p = foldMap (\x -> if p x then Seq.singleton x else mempty)

    pointIn fi = pointInteriorTo $ gr^?!interiorFacePolygonAt fi

    polygonsCoveringEdges d defs = let p        = midPoint d
                                       covering = filter' (midPoint d `intersects`) polygons
                                   in E defs covering p

    midPoint d = let ClosedLineSegment s t = (^.asPoint) <$> gr^?!edgeSegmentAt d
                 in s .+^ ((t .-. s) ^/ 2)

    polygonsCoveringVertices v = filter' (v `intersects`) polygons

-- I guess we could build point location structures on the polygons to
-- speed things up, if need be.
