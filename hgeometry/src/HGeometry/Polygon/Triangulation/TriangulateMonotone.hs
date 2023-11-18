--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Triangulation.TriangulateMonotone
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Polygon.Triangulation.TriangulateMonotone
  ( YMonotonePolygon_
  -- , triangulate
  -- , triangulate'
  , computeDiagonals
  -- , LR(..)
  -- , P
  -- , Stack
  -- , chainOf
  -- , toVtx
  -- , seg
  -- , process
  -- , isInside
  -- , mergeBy
  -- , splitPolygon
  ) where

-- import           HGeometry.Polygon.Triangulation.Types
import           Control.Lens
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (Down (..), comparing)
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.LineSegment
-- import           HGeometry.PlanarSubdivision.Basic (PlanarSubdivision, PolygonFaceData)
-- import           HGeometry.PlaneGraph (PlaneGraph)
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Algorithms.DivideAndConquer (mergeSortedListsBy)
import           HGeometry.Lens.Util
-- import           Hiraffe.Graph.Class

--------------------------------------------------------------------------------

-- | Y-monotone polygon. All straight horizontal lines intersects the polygon
--   no more than twice.
type YMonotonePolygon_ = SimplePolygon_

data LR = L | R deriving (Show,Eq)
{-

type PlanarSubdivision s point e f r = ()

-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate    :: ( YMonotonePolygon_ yMonotonePolygon point r
                  , Ord r, Fractional r)
               => yMonotonePolygon
               -> PlanarSubdivision s point PolygonEdgeType PolygonFaceData r
triangulate pg = constructSubdivision e es (computeDiagonals pg)
  where
    (e:es) = listEdges pg
  -- TODO: Find a way to construct the graph in O(n) time.

-- | Triangulates a polygon of \(n\) vertices
--
-- running time: \(O(n \log n)\)
triangulate'     :: forall s p r. (Ord r, Fractional r)
                 => yMonotonePolygon-> PlaneGraph s p PolygonEdgeType PolygonFaceData r
triangulate' pg' = constructGraph e es (computeDiagonals pg)
  where
    pg     = toCounterClockWiseOrder pg'
    (e:es) = listEdges pg
  -- TODO: Find a way to construct the graph in O(n) time.

-}

-- | Given a y-monotone polygon in counter clockwise order computes the diagonals
-- to add to triangulate the polygon
--
-- pre: the input polygon is y-monotone and has \(n \geq 3\) vertices
--
-- running time: \(O(n)\)
computeDiagonals    :: (YMonotonePolygon_ yMonotonePolygon point r, Ord r, Num r)
                    => yMonotonePolygon -> [ClosedLineSegment (point :+ VertexIx yMonotonePolygon)]
computeDiagonals pg = case unsnoc vs of
    Just (u:v:vs',w) -> go u v vs' w
    _                -> error "computeDiagonals. polygon should contain at least 3 vertices"
  where
    -- split the polygon into two chains of vertices (with the vertices in decreasing order)
    -- tag them with left and right, and merge the resulting list into one big decreasing list
    vs  = uncurry (mergeSortedListsBy $ comparing (\((Point2_ x y) :+ _) -> (Down y, x)))
        $ splitPolygon pg

    -- run the stack computation that actually triangulates the polygon.
    go u v vs' w = diags'' <> diags'
      where
        SP (_:|stack') diags' = List.foldl' (\(SP stack acc) v' -> (<> acc) <$> process v' stack)
                                            (SP (v :| [u]) []) vs'
        -- add vertices from the last guy w to all 'middle' guys of the final stack
        diags'' = map (seg w) $ init stack'

type P i point r = point :+ (i,LR)

-- | Get the chain of a particular point
chainOf :: P i point r -> LR
chainOf = view (extra._2)

-- | Produce a diagional
seg     :: P i point r -> P i point r -> ClosedLineSegment (point :+ i)
seg u v = ClosedLineSegment (toVtx u) (toVtx v)
  where
    toVtx = over extra fst

type Stack a = NonEmpty a

-- | The real triangulation procedure
process                    :: (Point_ point 2 r, Ord r, Num r)
                           => P i point r
                           -> Stack (P i point r)
                           -> SP (Stack (P i point r)) [ClosedLineSegment (point :+ i)]
process v stack@(u:|ws)
  | chainOf v /= chainOf u = SP (v:|[u])    (map (seg v) . NonEmpty.init $ stack)
  | otherwise              = SP (v:|w:rest) (map (seg v) popped)
      where
        (popped,rest) = over both (map fst) . List.span (isInside v)
                      $ zip ws (NonEmpty.toList stack)
        w             = NonEmpty.last $ u:|popped

-- | test if m does not block the line segment from v to u
isInside          :: (Point_ point 2 r, Ord r, Num r)
                  => P i point r
                  -> (P i point r, P i point r) -> Bool
isInside v (u, m) = case ccw (v^.core) (m^.core) (u^.core) of
                     CoLinear -> False
                     CCW      -> chainOf v == R
                     CW       -> chainOf v == L

-- | When the polygon is in counter clockwise order we return (leftChain,rightChain)
-- ordered from the top-down.
--
-- if there are multiple points with the maximum yCoord we pick the rightmost one,
-- if there are multiple point with the minimum yCoord we pick the leftmost one.
--
-- running time: \(O(n)\)
splitPolygon    :: (YMonotonePolygon_ yMonotonePolygon point r, Ord r)
                => yMonotonePolygon
                -> ( [point :+ (VertexIx yMonotonePolygon, LR)]
                   , [point :+ (VertexIx yMonotonePolygon, LR)]
                   )
splitPolygon pg = bimap (f L) (f R . reverse)
                . NonEmpty.break ((== vMinY) . fst)
                $ itoNonEmptyOf (ccwOuterBoundaryFrom vMaxY) pg
    -- rotates the list to the vtx with max ycoord, producing a
    -- non-empty list of elements, which we break at the point with
    -- minimum y,x coordinate.
  where
    f x = map (\(i,p) -> p :+ (i,x))
    vMaxY = first1Of (maximumVertexBy incYincX.withIndex._1) pg
    vMinY = first1Of (minimumVertexBy incYincX.withIndex._1) pg

    incYincX p q = comparing (^.yCoord) p q <> comparing (^.xCoord) p q

    -- Just vs' = CV.findRotateTo (\v -> v^.core == vMaxY)
    --          $ pg^.outerBoundaryVector

    -- vMaxY = getY F.maximumBy
    -- vMinY = getY F.minimumBy
    -- swap' (Point2 x y) = Point2 y x
    -- getY ff = let p = ff (comparing (^.core.to swap')) $ pg^.outerBoundaryVector
    --           in p^.core




--------------------------------------------------------------------------------

-- testPolygon = fromPoints . map ext $ [ Point2 10 10
--                                      , Point2 5 20
--                                      , Point2 3 14
--                                      , Point2 1 1
--                                      , Point2 8 8 ]






-- testPoly5 :: SimplePolygon () Rational
-- testPoly5 = toCounterClockWiseOrder . fromPoints $ map ext [ Point2 176 736
--                                                            , Point2 240 688
--                                                            , Point2 240 608
--                                                            , Point2 128 576
--                                                            , Point2 64 640
--                                                            , Point2 80 720
--                                                            , Point2 128 752
--                                                            ]


-- testPoly5 :: SimplePolygon () Rational
-- testPoly5 = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 320 320
--                                                              , Point2 256 320
--                                                              , Point2 224 320
--                                                              , Point2 128 240
--                                                              , Point2 64 224
--                                                              , Point2 256 192
--                                                              ]
