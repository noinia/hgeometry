--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.BatchPointLocation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of Batched point location among a set of planes.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.BatchedPointLocation
  ( batchedPointLocation
  , groupQueries
  ) where

import Data.Foldable1
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Vector qualified as Vector
import Control.Lens
import HGeometry.Plane
import HGeometry.Algorithms.BinarySearch
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Line.General
import HGeometry.Point
import HGeometry.Ext
import HGeometry.Foldable.Sort
import HGeometry.Combinatorial.Util
import HGeometry.Line.BatchPointLocation qualified as Line
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEMap
import HGeometry.PlaneGraph.Connected
import Data.Ord
import Data.Maybe
import Prelude hiding (lines)

--------------------------------------------------------------------------------

-- | Given a set of \(n\) queries, and a set of \(r\) planes H computes for each
-- query q the subset of planes above q, in increasing order.
--
-- running time: \(O(n\log n + r^5\log r)\)
batchedPointLocation                :: ( Point_ queryPoint 3 r
                                       , Plane_ plane r
                                       , Foldable set, Foldable1 set'
                                       , Ord r, Fractional r
                                       , Ord queryPoint
                                       )
                                    => set' queryPoint -> set plane
                                    -> NEMap.NEMap queryPoint (Vector.Vector plane)
batchedPointLocation queries planes = foldMap1 (answerBatch planes)
                                    $ groupQueries queries planes

--------------------------------------------------------------------------------

-- | Ansers a batch of queries
--
-- pre: the vertical (w.r.t z) order of the planes is the same for all queries in the batch
--
-- running time: \(O(n\log n + r\log r + K)\), where \(K\) is the output size.
--
answerBatch                :: forall set set' plane queryPoint r.
                              ( Foldable set, Foldable1 set'
                              , Point_ queryPoint 3 r
                              , Plane_ plane r
                              , Ord r, Num r
                              , Ord queryPoint
                              )
                           => set plane -> set' queryPoint
                           -> NEMap.NEMap queryPoint (Vector.Vector plane)
answerBatch planes queries = foldMap1 query queries
  where
    query q = NEMap.singleton q $ case binarySearchFirstIdxIn (q `liesBelow'`) sortedPlanes of
                                    Nothing -> Vector.empty
                                    Just i  -> Vector.drop i sortedPlanes

    q0 :: Point 2 r
    q0 = projectPoint $ (NonEmpty.head $ toNonEmpty queries)^.asPoint

    -- the planes, sorted from bottom to top
    sortedPlanes  :: Vector.Vector plane
    sortedPlanes  = sortOn (evalAt q0 :: plane -> r) planes

    liesBelow'       :: queryPoint -> plane -> Bool
    q `liesBelow'` h = verticalSideTest q h /= GT

--------------------------------------------------------------------------------


-- | Given a set of \(n\) query points, and the set of \(r\) planes,
-- partitions the queries into at most \(O(r^4)\) non-empty subsets Q_F so that
--
-- for every pair of query points q,q' in a set Q_F the vertical
-- (w.r.t z) line through q intersects the planes in the input set in
-- the same order as the vertical line through q'.
--
-- running time: \(O(n\log n + r^4 \log r)\)
groupQueries                :: ( Point_ queryPoint 3 r
                               , Plane_ plane r
                               , Foldable set, Foldable1 nonEmpty
                               , Ord r, Fractional r
                               )
                            => nonEmpty queryPoint -> set plane
                            -> NonEmpty (NonEmpty queryPoint)
groupQueries queries planes = case NonEmpty.nonEmpty lines of
    Nothing     -> NonEmpty.singleton (toNonEmpty queries) -- apparently all planes are parallel
    Just lines' -> fmap (fmap (^.extra)) . toNonEmpty
                 $ Line.groupQueries queries' lines'
  where
    lines    = mapMaybe (\(Two h1 h2) -> projectedIntersectionLine h1 h2) $ uniquePairs planes
    queries' = fmap (\q -> (projectPoint (q^.asPoint) :+ q)) . toNonEmpty $ queries

    -- vertices = mapMaybe asVertex $ uniquePairs lines



    -- asVertex (Two l1 l2) = l1 `intersect` l2  >>= \case
    --                          Line_x_Line_Point v -> Just $ vertexEvent v l1 l2
    --                          _                   -> Nothing

    -- events = sortOn eventTime ((Query <$> NonEmpty.toList queries) <> vertices)

    -- -- | Initializes the status structure
    -- initialize   :: Event r queryPoint line -> (Set plane, [NonEmpty queryPoint])
    -- initialize e = let x   = eventTime e - 1
    --                    ss  = undefined -- order the planes at x
    --                in handle e (ss, [])

    -- handle :: (Set plane, [NonEmpty queryPoint])
    --        -> Event r queryPoint line
    --        -> (Set plane, [NonEmpty queryPoint])
    -- handle e (ss, acc) = case e of
    --   Query q -> undefined
    --   Vertex v l1 l2 -> undefined -- actually, this could agian be an entire buch of lines


--------------------------------------------------------------------------------
-- data Event r query line = Query query
--                         | Vertex !(Point 2 r)
--                                  line -- ^ the line that is lower on the left
--                                  line -- ^ the line that is lower on the right

-- -- | Get the "time" at which the event occurs
-- eventTime :: Point_ query 3 r => Event r query line -> r
-- eventTime = \case
--   Query q      -> q^.xCoord
--   Vertex v _ _ -> v^.xCoord


-- -- | Construct an event
-- vertexEvent                       :: forall point line query r.
--                                      ( Num r
--                                      , Line_ line 2 r
--                                      )
--                                    => Point 2 r -> line -> line -> Event r query line
-- vertexEvent v l1 l2 | f l1 < f l2 = Vertex v l1 l2
--                     | otherwise   = Vertex v l2 l1
--   where
--     f   :: line -> r
--     f l = evalAt (Point $ v^.xCoord - 1) l





--------------------------------------------------------------------------------
