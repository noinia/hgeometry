module Data.Geometry.Ipe.FromIpe where

import qualified Data.Traversable as Tr
import Data.Ext
import Data.Geometry.Ipe.Types
import Data.Geometry.Line
import Data.Geometry.LineSegment
import qualified Data.Geometry.PolyLine as PolyLine
import Data.Geometry.Polygon
import Control.Lens
import qualified Data.Seq2 as S2


-- | Try to convert a path into a line segment, fails if the path is not a line
-- segment or a polyline with more than two points.
asLineSegment :: Prism' (Path r) (LineSegment 2 () r)
asLineSegment = prism' seg2path path2seg
  where
    seg2path   = review asPolyLine . PolyLine.fromLineSegment
    path2seg p = PolyLine.asLineSegment' =<< preview asPolyLine p


-- | Convert to a polyline. Ignores all non-polyline parts
asPolyLine :: Prism' (Path r) (PolyLine.PolyLine 2 () r)
asPolyLine = prism' poly2path path2poly
  where
    poly2path = Path . S2.l1Singleton  . PolyLineSegment
    path2poly = preview (pathSegments.Tr.traverse._PolyLineSegment)
    -- TODO: Check that the path actually is a polyline, rather
    -- than ignoring everything that does not fit

-- | Convert to a simple polygon: simply takes the first closed path
asSimplePolygon :: Prism' (Path r) (SimplePolygon () r)
asSimplePolygon = prism' poly2path path2poly
  where
    poly2path = Path . S2.l1Singleton . PolygonPath
    path2poly = preview (pathSegments.Tr.traverse._PolygonPath)
    -- TODO: Check that the path actually is a simple polygon, rather
    -- than ignoring everything that does not fit
