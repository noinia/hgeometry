module HGeometry.DelaunayTriangulation.ViaLowerEnvelope
  ( delaunayTriangulation
  ) where

import HGeometry.HyperPlane.NonVertical
import HGeometry.LowerEnvelope
import HGeometry.Point
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Computes the triangles in the delaunay triangulation by lifting
-- the points to planes in R^3, and computing their lower envelope.
--
-- pre: at least three points, the points form a set, and the points
-- are in general position.
delaunayTriangulation     :: (Point_ point 2 r
                             ) => f point -> [Vector 3 (Point 2 r)]
                          -- todo, return 'point' instead.
delaunayTriangulation pts = undefined
  where
    env = lowerEnvelope planes
    planes = liftPoint <$> pts
