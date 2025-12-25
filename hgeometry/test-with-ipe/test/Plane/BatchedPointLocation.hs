module Plane.BatchedPointLocation
  ( batchedPointLocation
  , groupQueries
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Control.Lens
import HGeometry.Plane.LowerEnvelope.Connected.Primitives
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Line.General
import HGeometry.Point
import HGeometry.Foldable.Sort
import HGeometry.Set.Util qualified as SS -- status struct
import Data.Set qualified as SS -- status struct

--------------------------------------------------------------------------------



-- | Given a set of \(n\) queries, and a set of \(r\) planes H computes for each
-- query q the subset of planes above q, in increasing order.
--
-- running time: \(O(n\log n + r^5\log r)\)
batchedPointLocation                :: ( Point_ queryPoint 3 r
                                       , Plane_ plane r
                                       , Foldable set
                                       , Ord r, Num r
                                       )
                                    => NonEmpty queryPoint -> set plane -> NonEmpty (Int, [plane])
batchedPointLocation queries planes = foldMap1 (answerBatch planes)
                                    $ groupQueries (zipWith (:+) queries [0..]) planes

--------------------------------------------------------------------------------

-- | Ansers a batch of queries
--
-- pre: the vertical (w.r.t z) order of the planes is the same for all queries in the batch
--
-- running time: \(O(n\log n + r\log r + K)\), where \(K\) is the output size.
--
answerBatch                :: ( Foldable set
                              , Point_ queryPoint 3 r
                              , Plane_ plane r
                              , Ord r, Num r
                              )
                           => set plane -> NonEmpty (queryPoint :+ Int) -> NonEmpty (Int, [plane])
answerBatch planes queries = scan queries' (toList planes')
  where
    -- the planes, sorted from bottom to top
    planes'  = sortOn (evalAt (projectPoint $ NonEmpty.head queries)) planes
    -- the queries, sorted from bottom to top
    queries' = NonEmpty.sortWith (^.zCoord) queries

    -- essentially merge and output the lists. for eqch query we output the remaining list.
    scan                      :: NonEmpty (queryPoint :+ Int) -> [plane]  -> NonEmpty (Int,[plane])
    scan qs@((q :+ i) :| qs') = \case
      []                           -> fmap (\(q' :+ i') -> (i',[])) qs
      hs@(h:hs') | q `liesBelow` h -> (i, hs) :| scan' qs' hs
                 | oterwise        -> scan qs hs'

    scan' qs hs = case NonEmpty.nonEmpty qs of
                    Nothing  -> []
                    Just qs' -> NonEmpty.toList $ scan qs' hs

    q `liesBelow` h = verticalSideTest q h /= GT


--------------------------------------------------------------------------------


-- | Given a set of \(n\) query points, and the set of \(r\) planes,
-- partitions the queries into at most \(O(r^4)\) non-empty subsets Q_F so that
--
-- for every pair of query points q,q' in a set Q_F the vertical
-- (w.r.t z) line through q intersects the planes in the input set in
-- the same order as the vertical line through q'.
--
-- running time: \(O(n\log n + r^4 \log r)\)
groupQueries                :: (

                               )
                            => NonEmpty queryPoint -> set plane -> NonEmpty (NonEmpty queryPoint)
groupQueries queries planes =



  NonEmpty.fromlist . snd $ foldlMap1' initialize handle events
                              -- the fromList is safe, since each query is an event, and such
                              -- an event prdocues output
  where
    lines    = mapMaybe (\(Two h1 h2) -> intersectionLine h1 h2) $ uniquePairs planes





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
data Event r query line = Query query
                        | Vertex !(Point 2 r)
                                 line -- ^ the line that is lower on the left
                                 line -- ^ the line that is lower on the right

-- | Get the "time" at which the event occurs
eventTime :: Point_ query 3 r => Event r query line -> r
eventTime = \case
  Query q      -> q^.xCoord
  Vertex v _ _ -> v^.xCoord


-- | Construct an event
vertexEvent                       :: Point 2 r -> line -> line -> Event r query line
vertexEvent v l1 l2 | f l1 < f l2 = Vertex v l1 l2
                    | otherwise   = Vertex v l2 l1
  where
    f l = evalAt (v^.xCoord - 1) l





--------------------------------------------------------------------------------
sortOn = sortBy . comparing
