{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone where


import           Control.Lens
import           Control.Monad (forM_, when)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer (WriterT, execWriterT,tell)
import qualified Data.BalBST as SS
import           Data.Bifunctor
import qualified Data.CircularSeq as C
import qualified Data.DList as DList
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Functor.Contravariant
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup
import           Data.Tuple (swap)
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import           Debug.Trace

--
type MonotonePolygon p r = SimplePolygon p r

triangulate :: (Ord r, Num r) => MonotonePolygon p r -> [LineSegment 2 p r]
triangulate = undefined


data LR = L | R deriving (Show,Eq)


-- | Given a y-monotone polygon in counter clockwise order computes the diagonals
-- to add to triangulate the polygon
--
-- pre: the input polygon is y-monotone and has \(n \geq 3\) vertices
--
-- running time: \(O(n)\)
computeDiagonals    :: (Ord r, Num r, Show r, Show p)
                    => MonotonePolygon p r -> [LineSegment 2 p r]
computeDiagonals pg = diags'' <> diags'
  where
    -- | run the stack computation
    SP (_:stack') diags' = L.foldl' (\(SP stack acc) v -> (<> acc) <$> process v stack)
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

process                    :: (Ord r, Num r, Show r, Show p)
                           => P p r -> Stack (P p r)
                           -> SP (Stack (P p r)) [LineSegment 2 p r]
process v stack | traceShow ("process", v, " ", stack) False = undefined
process v stack@(u:ws)
  | chainOf v /= chainOf u = SP [v,u]         (map (seg v) . init $ stack)
  | otherwise              = SP (v:w <> rest) (map (seg v) diags)
      where
        (diags,rest) = bimap (map fst) (map fst) . L.span (isInside v) $ zip ws stack
        w            = take 1 . reverse $ diags

-- | test if m blocks the line segment from v to u
isInside          :: (Ord r, Num r) => P p r -> (P p r, P p r) -> Bool
isInside v (u, m) = case ccw' v m u of
                     CoLinear -> True
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
-- ordered from the top-down
splitPolygon    :: Ord r => MonotonePolygon p r
                -> ([Point 2 r :+ (LR :+ p)], [Point 2 r :+ (LR :+ p)])
splitPolygon pg = swap . bimap (f R) (f L) . second reverse
                . L.break (pred (<=)) . F.toList . C.leftElements $ vs'
  where
    f x = map ((&extra %~ (x :+)) . (^.extra._1.end))

    pred cmp (v :+ SP a b) = let vy = v^.yCoord
                                 ay = a^.start.core.yCoord
                                 by = b^.end.core.yCoord
                             in vy `cmp` ay && vy `cmp` by

    Just vs' = C.findRotateTo (pred (>=)) $ (withIncidentEdges pg) ^.outerBoundary

--------------------------------------------------------------------------------

testPolygon = fromPoints . map ext $ [ point2 10 10
                                     , point2 5 20
                                     , point2 3 14
                                     , point2 1 1
                                     , point2 8 8 ]
