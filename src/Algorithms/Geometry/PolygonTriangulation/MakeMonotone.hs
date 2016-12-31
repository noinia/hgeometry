module Algorithms.Geometry.PolygonTriangulation.MakeMonotone where

import Data.Ext
import Data.CircularSeq (rotateL, rotateR, zip3LWith)
import Control.Lens
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Ord (comparing, Down(..))
import Data.Semigroup

--------------------------------------------------------------------------------

data VertexType = Start | Merge | Split | End | Regular deriving (Show,Read,Eq)


-- | assigns a vertex type to each vertex
--
-- pre: the polygon is given in CCW order
--
-- running time: $O(n)$.
classifyVertices                   :: (Num r, Ord r
                                      , Show r, Show p
                                      )
                                    => SimplePolygon p r
                                    -> SimplePolygon (p :+ VertexType) r
classifyVertices (SimplePolygon vs) =
    SimplePolygon $ zip3LWith f (rotateL vs) vs (rotateR vs)
  where
    -- is the angle larger than > 180 degrees
    largeInteriorAngle p c n = case ccw (p^.core) (c^.core) (n^.core) of
           CCW -> False
           CW  -> True
           _   -> error "classifyVertices -> largeInteriorAngle: colinear points"

    f p c n = c&extra %~ (:+ vt)
      where
        vt = case (p `cmpSweep` c, n `cmpSweep` c, largeInteriorAngle p c n) of
               (LT, LT, False) -> Start
               (LT, LT, True)  -> Split
               (GT, GT, False) -> End
               (GT, GT, True)  -> Merge
               _               -> Regular

-- | p < q = p.y < q.y || p.y == q.y && p.x > q.y
cmpSweep :: Ord r => Point 2 r :+ e -> Point 2 r :+ e -> Ordering
p `cmpSweep` q =
  comparing (^.core.yCoord) p q <> comparing (Down . (^.core.xCoord)) p q


--------------------------------------------------------------------------------

-- fromAdjacencyList

makeMonotone      :: SimplePolygon p r -> proxy s -> PlanarSubdivision s p () Bool
makeMonotone pg _ = sweep .


sweep  = undefined





--------------------------------------------------------------------------------


testPoly = fromPoints . map ext $ [ origin
                                  , point2 0 (-1)
                                  , point2 0 (-5)
                                  , point2 1 (-5)
                                  , point2 5 (-1)
                                  , point2 7 (-4)
                                  , point2 9 1
                                  , point2 6 4
                                  , point2 3 2
                                  , point2 1 5
                                  ]
testAnswer = [Regular, Regular, Regular, End, Split, End, Regular, Start, Merge, Start]


testP = point2 7 2
testQ = point2 3 2

-- (Point2 px py :+ _) `pntOrd` (Point2 qx qy :+ ) =
--  comparing


--     case (py `compare` qy, px `compare` qx) of
--       (LT,_)  -> LT
--       (EQ,GT) -> LT
--       _       -> False





-- (Point2 px py :+ _) `isBelow` (Point2 qx qy :+ ) =
--     case (py `compare` qy, px `compare` qx) of
--       (LT,_)  -> True
--       (EQ,GT) -> True
--       _       -> False


-- (Point2 px py :+ ) `isAbove` (Point2 qx qy :+ ) =
--     case (py `compare` qy, px `compare` qx) of
--       (GT,_)  -> True
--       (EQ,LT) -> True
--       _       -> False
