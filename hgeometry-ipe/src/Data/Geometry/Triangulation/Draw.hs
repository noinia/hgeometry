module Data.Geometry.Triangulation.Draw where

import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Data.Geometry.LineSegment
import           Ipe

--------------------------------------------------------------------------------

-- | Draws a triangulation
drawTriangulation :: IpeOut (Triangulation p r) Group r
drawTriangulation tr =
  ipeGroup [ iO $ ipeLineSegment e
           | e <- map (uncurry ClosedLineSegment) . edgesAsPoints $ tr
           ]
