module Algorithms.Geometry.PolygonTriangulation.MakeMonotone where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Control.Lens
import qualified Data.BalBST as SS
import           Data.CircularSeq (rotateL, rotateR, zip3LWith)
import           Data.Ext
import           Data.Functor.Contravariant
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup
import           Data.Util

--------------------------------------------------------------------------------

data VertexType = Start | Merge | Split | End | Regular deriving (Show,Read,Eq)


-- | assigns a vertex type to each vertex
--
-- pre: the polygon is given in CCW order
--
-- running time: $O(n)$.
classifyVertices                   :: (Num r, Ord r)
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

makeMonotone      :: SimplePolygon p r -> proxy s -> PlanarSubdivision s p () Bool r
makeMonotone pg _ = undefined

-- type StatusStruct r = SS.BalBST r (LineSegment 2 p r :+ e)

ordAtNav :: (Fractional r, Ord r) => r -> SS.TreeNavigator r (LineSegment 2 p r :+ e)
ordAtNav = contramap (view core) . BO.ordAtNav

type StatusStruct r p e = SS.BalBST r (LineSegment 2 p r :+ e)

type Helper p r = Point 2 r :+ (p :+ VertexType)

type StatusStruct' p r = StatusStruct r p (Helper p r)


helper :: Lens' (LineSegment 2 p r :+ e) e
helper = extra

vertexType :: Lens' (Helper p r) VertexType
vertexType = extra.extra

helper'   :: LineSegment 2 p r :+ (Helper p r) -> Point 2 r :+ p
helper' e = (e^.helper) &extra %~ view core



-- | The
withIncidentEdges                    :: SimplePolygon p r
                                     -> SimplePolygon (Two (LineSegment 2 p r)) r
withIncidentEdges (SimplePolygon vs) =
    SimplePolygon $ zip3LWith f (rotateL vs) vs (rotateR vs)
  where
    seg p q = LineSegment (Closed p) (Open q)
    f p c n = c&extra .~ SP (seg p c) (seg p n)


findDiagonals  :: (Fractional r, Ord r) => SimplePolygon p r -> [LineSegment 2 p r]
findDiagonals = view _1 . sweep . NonEmpty.sortBy cmpSweep
              . polygonVertices . withIncidentEdges . classifyVertices
  where
    sweep :: (Fractional r, Ord r)
          => NonEmpty.NonEmpty (Event p r) -> SP [LineSegment 2 p r] (StatusStruct' p r)
    sweep = foldr handle (SP [] (SS.empty $ ordAtNav undefined))

getEvent :: Two (LineSegment 2 (p :+ vt) r) -> vt
getEvent = view (_1.end.extra.extra)





type Event p r = Point 2 r :+ (Two (LineSegment 2 (p :+ VertexType) r))



handle               :: (Fractional r, Ord r)
                     => Event p r
                     -> SP [LineSegment 2 p r] (StatusStruct' p r)
                     -> SP [LineSegment 2 p r] (StatusStruct' p r)
handle (p :+ adj) ss@(SP diags t) = case getEvent adj of
    Start   -> SP diags (SS.insert (ei :+ pv) t')
    End     -> let e  = undefined
                   md = undefined
               in SP (md <> diags) (SS.delete e t')
    Split   -> let e = undefined
                   d = ClosedLineSegment (p :+ x) (e^.helper)
               in SP (d : diags) (SS.insert (ei :+ pv) t')
    Merge   -> undefined
    Regular -> undefined
  where
    ei = bimap (^.core) id $ adj^._2

    x   = adj^._1.end.extra.core
    pv  = p :+ (x :+ getEvent adj)
    t'  = t { SS.nav = ordAtNav $ p^.yCoord }


-- handle e@(getEvent -> p :+ Start) ss = undefined


--         :+ (e :+ Start)) ss =





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
