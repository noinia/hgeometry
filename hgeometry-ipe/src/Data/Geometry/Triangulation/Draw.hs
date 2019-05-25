module Data.Geometry.Triangulation.Draw where

import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Data.Geometry.LineSegment
import           Data.Geometry.Ipe

--------------------------------------------------------------------------------

-- | Draws a triangulation
drawTriangulation :: IpeOut (Triangulation p r) Group r
drawTriangulation tr =
  ipeGroup [ iO $ ipeLineSegment e
           | e <- map (uncurry ClosedLineSegment) . triangulationEdges $ tr
           ]
