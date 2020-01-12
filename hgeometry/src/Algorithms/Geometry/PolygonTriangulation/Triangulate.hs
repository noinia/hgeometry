module Algorithms.Geometry.PolygonTriangulation.Triangulate where


import qualified Algorithms.Geometry.PolygonTriangulation.MakeMonotone as MM
import qualified Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone as TM
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Control.Lens
import           Data.Either (lefts)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.Polygon
import           Data.PlaneGraph (PlaneGraph)

--------------------------------------------------------------------------------

-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate        :: (Ord r, Fractional r)
                   => proxy s -> Polygon t p r
                   -> PlanarSubdivision s p PolygonEdgeType PolygonFaceData r
triangulate px pg' = constructSubdivision px e es diags
  where
    (pg, diags)   = computeDiagonals' pg'
    (e:es)        = listEdges pg


-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate'        :: (Ord r, Fractional r)
                    => proxy s -> Polygon t p r
                    -> PlaneGraph s p PolygonEdgeType PolygonFaceData r
triangulate' px pg' = constructGraph px e es diags
  where
    (pg, diags)   = computeDiagonals' pg'
    (e:es)        = listEdges pg


-- | Computes a set of diagaonals that together triangulate the input polygon
-- of \(n\) vertices.
--
-- running time: \(O(n \log n)\)
computeDiagonals :: (Ord r, Fractional r) => Polygon t p r -> [LineSegment 2 p r]
computeDiagonals = snd . computeDiagonals'

-- | Computes a set of diagaonals that together triangulate the input polygon
-- of \(n\) vertices. Returns a copy of the input polygon, whose boundaries are
-- oriented in counter clockwise order, as well.
--
-- running time: \(O(n \log n)\)
computeDiagonals'     :: (Ord r, Fractional r)
                      => Polygon t p r -> (Polygon t p r, [LineSegment 2 p r])
computeDiagonals' pg' = (pg, monotoneDiags <> extraDiags)
  where
    pg            = toCounterClockWiseOrder pg'
    monotoneP     = MM.makeMonotone (Identity pg) pg -- use some arbitrary proxy type
    -- outerFaceId'  = outerFaceId monotoneP

    monotoneDiags = map (^._2.core) . filter (\e' -> e'^._2.extra == Diagonal)
                  . F.toList . edgeSegments $ monotoneP
    extraDiags    = concatMap (TM.computeDiagonals . toCounterClockWiseOrder')
                  . lefts . map (^._2.core)
                  . filter (\mp -> mp^._2.extra == Inside) -- triangulate only the insides
                  -- . filter (\f -> f^._1 /= outerFaceId')
                  . F.toList . rawFacePolygons $ monotoneP

    -- -- we alredy know we get the polgyons in *clockwise* order, so skip the
    -- -- check if it is counter clockwise
    -- toCounterClockWiseOrder'' = reverseOuterBoundary
