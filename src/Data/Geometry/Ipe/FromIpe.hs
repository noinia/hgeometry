module Data.Geometry.Ipe.FromIpe where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.Polygon
import qualified Data.Seq2 as S2
import qualified Data.Traversable as Tr


-- | Try to convert a path into a line segment, fails if the path is not a line
-- segment or a polyline with more than two points.
_asLineSegment :: Prism' (Path r) (LineSegment 2 () r)
_asLineSegment = prism' seg2path path2seg
  where
    seg2path   = review _asPolyLine . PolyLine.fromLineSegment
    path2seg p = PolyLine.asLineSegment' =<< preview _asPolyLine p


-- | Convert to a polyline. Ignores all non-polyline parts
_asPolyLine :: Prism' (Path r) (PolyLine.PolyLine 2 () r)
_asPolyLine = prism' poly2path path2poly
  where
    poly2path = Path . S2.l1Singleton  . PolyLineSegment
    path2poly = preview (pathSegments.Tr.traverse._PolyLineSegment)
    -- TODO: Check that the path actually is a polyline, rather
    -- than ignoring everything that does not fit

-- | Convert to a simple polygon: simply takes the first closed path
_asSimplePolygon :: Prism' (Path r) (SimplePolygon () r)
_asSimplePolygon = prism' poly2path path2poly
  where
    poly2path = Path . S2.l1Singleton . PolygonPath
    path2poly = preview (pathSegments.Tr.traverse._PolygonPath)
    -- TODO: Check that the path actually is a simple polygon, rather
    -- than ignoring everything that does not fit

-- | use the first prism to select the ipe object to depicle with, and the second
-- how to select the geometry object from there on. Then we can select the geometry
-- object, directly with its attributes here.
_withAttrs       :: Prism' (IpeObject r) (i r :+ IpeAttributes i r) -> Prism' (i r) g
                 -> Prism' (IpeObject r) (g :+ IpeAttributes i r)
_withAttrs po pg = prism' g2o o2g
  where
    g2o    = review po . over core (review pg)
    o2g o  = preview po o >>= \(i :+ ats) -> (:+ ats) <$> preview pg i
