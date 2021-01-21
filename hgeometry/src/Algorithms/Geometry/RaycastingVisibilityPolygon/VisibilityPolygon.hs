module Algorithms.Geometry.VisibilityPolygon.Naive where

import Data.Geometry.Polygon.Core
import Data.Geometry.Point
import Data.Geometry.HalfLine
import Data.Geometry.LineSegment
import Data.Vector.Circular


{- Some interesting cases to consider:

Case 1: Ray from 'x' hits a corner and is blocked.

     /|\
    / | \
   /  |  \
  /   |   \
 \----x----/

Case 2: Ray from 'x' hits a vertex on the right side and isn't blocked:
/---------\
|       | /
|       |/
|       |\
|       | \
|       |  |
\-------x--/

Case 3: Ray from 'x' hits a vertex on the left side and isn't blocked:
/---------\
 \ |      |
  \|      |
  /|      |
 / |      |
|  |      |
\--x------/


Case 4: Ray from 'x' to a far corner is blocked by edges.
   /------\
  /|      |
 /----\   |
   |  |   |
/-----/   |
|  |      |
\--x------/


Case 5: Ray from 'x' is squeezed on both sides. Be careful you don't allow zero-width slivers
        in the final visibility polygon.

/-------\
|       |
|   /---/
|   |     <- Ray should be blocked by this S-bend.
|   \---\
|   |   |
\---\   |
    |   | <- Ray should pass through first S-bend.
/---/   |
|   |   |
\---x---/



The brute-force visibility algorithm works as follows:
1. X is a given point of origin inside of P.
2. Sort the vertices of P ccw arount X.
3. Shoot a ray from X to each vertex in P and collect any intersections.
4. For each ray, sort the intersections by distance to X. Then:
  If the ray hits the vertex without any other intersections /and/
  the ray is blocked by the vertex (see case 1 drawing), then
  the vertex is in the visibility polygon.
  If the ray grazes a point on the right side (see case 2) then
  both the grazed vertex AND the final intersection point are
  in the visibility polygon.
  Case 3 is similar to case 2 with the only difference that the
  grazing vertex and the final intersection point should be reversed
  in the visibility polygon. Ie the final intersection point should
  appear before the grazing vertex.
  For case 4, nothing needs to be done and no points along the ray
  should appear in the visibility polygon.
5. Take the points we know are in the visiblity polygon and construct
   a polygon. The points are already in the right order (because of step 2)
   so we can simply call 'fromPoints'.

-}


visibility :: SimplePolygon p r -> Point 2 r -> SimplePolygon () r
visibility polygon point = 
    error "Not yet implemented"


-------------------------------------------------------------------------------
-- Helper functions

data Event p r
  = SegmentIntersection (Point 2 r) (LineSegment 2 p r)
  | VertexIntersection (Point 2 r) (LineSegment 2 p r) (LineSegment 2 p r)

shootRay :: HalfLine 2 r -> CircularVector (LineSegment 2 p r) -> [Event p r]
shootRay = undefined
