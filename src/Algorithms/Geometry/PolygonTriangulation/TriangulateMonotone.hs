module Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone where

import           Control.Lens
import           Data.Bifunctor
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List as L
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup
import           Data.Tuple (swap)
import           Data.Util
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Data.PlaneGraph (PlaneGraph)
import           Data.Geometry.PlanarSubdivision.Basic(PolygonFaceData, PlanarSubdivision)

--------------------------------------------------------------------------------

--
type MonotonePolygon p r = SimplePolygon p r

data LR = L | R deriving (Show,Eq)

-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate        :: (Ord r, Fractional r)
                   => proxy s -> MonotonePolygon p r
                   -> PlanarSubdivision s p PolygonEdgeType PolygonFaceData r
triangulate px pg' = constructSubdivision px e es (computeDiagonals pg)
  where
    pg     = toCounterClockWiseOrder pg'
    (e:es) = listEdges pg
  -- TODO: Find a way to construct the graph in O(n) time.

-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate'        :: (Ord r, Fractional r)
                    => proxy s -> MonotonePolygon p r
                    -> PlaneGraph s p PolygonEdgeType PolygonFaceData r
triangulate' px pg' = constructGraph px e es (computeDiagonals pg)
  where
    pg     = toCounterClockWiseOrder pg'
    (e:es) = listEdges pg
  -- TODO: Find a way to construct the graph in O(n) time.


-- | Given a y-monotone polygon in counter clockwise order computes the diagonals
-- to add to triangulate the polygon
--
-- pre: the input polygon is y-monotone and has \(n \geq 3\) vertices
--
-- running time: \(O(n)\)
computeDiagonals    :: (Ord r, Num r)
                    => MonotonePolygon p r -> [LineSegment 2 p r]
computeDiagonals pg = diags'' <> diags'
  where
    -- | run the stack computation
    SP (_:stack') diags' = L.foldl' (\(SP stack acc) v' -> (<> acc) <$> process v' stack)
                                    (SP [v,u] []) vs'
    -- add vertices from the last guy w to all 'middle' guys of the final stack
    diags'' = map (seg w) $ init stack'
    -- extract the last vertex
    Just (vs',w) = unsnoc vs
    -- merge the two lists into one list for procerssing
    (u:v:vs) = uncurry (mergeBy $ comparing (\(Point2 x y :+ _) -> (Down y, x)))
             $ splitPolygon pg


type P p r = Point 2 r :+ (LR :+ p)

type Stack a = [a]




-- type Scan p r = State (Stack (P p r))

chainOf :: P p r -> LR
chainOf = (^.extra.core)

toVtx :: P p r -> Point 2 r :+ p
toVtx = (&extra %~ (^.extra))

seg     :: P p r -> P p r -> LineSegment 2 p r
seg u v = ClosedLineSegment (toVtx u) (toVtx v)

process                    :: (Ord r, Num r)
                           => P p r -> Stack (P p r)
                           -> SP (Stack (P p r)) [LineSegment 2 p r]
process _ []               = error "TriangulateMonotone.process: absurd. empty stack"
process v stack@(u:ws)
  | chainOf v /= chainOf u = SP [v,u]      (map (seg v) . init $ stack)
  | otherwise              = SP (v:w:rest) (map (seg v) popped)
      where
        (popped,rest) = bimap (map fst) (map fst) . L.span (isInside v) $ zip ws stack
        w             = last $ u:popped


-- | test if m does not block the line segment from v to u
isInside          :: (Ord r, Num r) => P p r -> (P p r, P p r) -> Bool
isInside v (u, m) = case ccw' v m u of
                     CoLinear -> False
                     CCW      -> chainOf v == R
                     CW       -> chainOf v == L

-- | given a comparison function, merge the two ordered lists
mergeBy     :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp = go
  where
    go []     ys     = ys
    go xs     []     = xs
    go (x:xs) (y:ys) = case x `cmp` y of
                         GT -> y : go (x:xs) ys
                         _  -> x : go xs     (y:ys)


-- | When the polygon is in counter clockwise order we return (leftChain,rightChain)
-- ordered from the top-down.
--
-- if there are multiple points with the maximum yCoord (or minimumYCoord) we
-- split the polygon at some arbitary one(s).
--
-- running time: \(O(n)\)
splitPolygon    :: Ord r => MonotonePolygon p r
                -> ([Point 2 r :+ (LR :+ p)], [Point 2 r :+ (LR :+ p)])
splitPolygon pg = swap
                . bimap (f R) (f L)
                . second reverse
                . L.break (\v -> v^.core.yCoord == vMinY)
                . F.toList . C.leftElements $ vs'
  where
    f x = map (&extra %~ (x :+))
    -- rotates the list to the vtx with max ycoord
    Just vs' = C.findRotateTo (\v -> v^.core.yCoord == vMaxY)
             $ pg^.outerBoundary
    -- find the y coord of the vtx with maximum and minimum YCoord note that we
    -- are not using fmap (^.core.yCoord) here since that means we kind of need
    -- a lazy fmap (depending on what DS the boundary is stored in).
    vMaxY = getY F.maximumBy
    vMinY = getY F.minimumBy
    getY ff = let p = ff (comparing (^.core.yCoord)) $ pg^.outerBoundary
              in p^.core.yCoord

--------------------------------------------------------------------------------

-- testPolygon = fromPoints . map ext $ [ point2 10 10
--                                      , point2 5 20
--                                      , point2 3 14
--                                      , point2 1 1
--                                      , point2 8 8 ]

testPoly5 :: SimplePolygon () Rational
testPoly5 = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 352 384
                                                             , Point2 128 176
                                                             , Point2 224 320
                                                             , Point2 48 400
                                                             , Point2 160 384
                                                             ]
