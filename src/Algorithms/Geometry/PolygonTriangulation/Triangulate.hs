module Algorithms.Geometry.PolygonTriangulation.Triangulate where


import qualified Algorithms.Geometry.PolygonTriangulation.MakeMonotone as MM
import qualified Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone as TM
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Control.Lens
import           Data.Bifunctor
import qualified Data.CircularSeq as C
import           Data.Either (lefts)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (listToMaybe)
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup
import           Data.Tuple (swap)
import           Data.Util


triangulate       :: (Ord r, Fractional r, Show p, Show r)
                  => proxy s -> Polygon t p r
                  -> PlanarSubdivision s p PolygonEdgeType PolygonFaceData r
triangulate px pg = constructSubdivision px e es diags
  where
    (e:es)        = listEdges pg
    monotoneP     = MM.makeMonotone (wrap px) pg
    outerFaceId'  = outerFaceId monotoneP

    monotoneDiags = map (^._2.core) . filter (\e' -> e'^._2.extra == Diagonal)
                  . edgeSegments $ monotoneP
    extraDiags    = concatMap TM.computeDiagonals
                  . lefts . map (^._2.core)
                  . filter (\f -> f^._1 == outerFaceId')
                  . F.toList . rawFacePolygons $ monotoneP
    diags         = monotoneDiags <> extraDiags

    -- just to make sure that the proxies have different types
wrap :: a -> Identity a
wrap = Identity
