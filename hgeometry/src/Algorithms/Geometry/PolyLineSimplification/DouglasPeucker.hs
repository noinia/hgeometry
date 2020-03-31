module Algorithms.Geometry.PolyLineSimplification.DouglasPeucker where

import           Control.Lens hiding (only)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Vector
import qualified Data.LSeq as LSeq
import           Data.LSeq (LSeq, pattern (:|>))
import           Data.Ord (comparing)

--------------------------------------------------------------------------------

-- | Line simplification with the well-known Douglas Peucker alogrithm. Given a distance
-- value eps adn a polyline pl, constructs a simplification of pl (i.e. with
-- vertices from pl) s.t. all other vertices are within dist eps to the
-- original polyline.
--
-- Running time: O(n^2) worst case, O(n log n) expected.
douglasPeucker         :: (Ord r, Fractional r, Arity d)
                       => r -> PolyLine d p r -> PolyLine d p r
douglasPeucker eps pl
    | dst <= (eps*eps) = fromPointsUnsafe [a,b] -- at least two points, so we are fine.
    | otherwise        = douglasPeucker eps pref `merge` douglasPeucker eps subf
  where
    pts         = pl^.points
    a           = LSeq.head pts
    b           = LSeq.last pts
    (i,dst)     = maxDist pts (ClosedLineSegment a b)

    (pref,subf) = split i pl

--------------------------------------------------------------------------------
-- * Internal functions

-- | Concatenate the two polylines, dropping their shared vertex
merge          :: PolyLine d p r -> PolyLine d p r -> PolyLine d p r
merge pref sub = PolyLine $ pref' `append` (sub^.points)
  where
    (pref' :|> _) = pref^.points
    append     :: LSeq.LSeq n a ->  LSeq.LSeq m a -> LSeq.LSeq m a
    append a b = LSeq.promise $ LSeq.append a b
    -- our seq is actually even longer


-- | Split the polyline at the given vertex. Both polylines contain this vertex
split                  :: Int -> PolyLine d p r
                       -> (PolyLine d p r, PolyLine d p r)
split i (PolyLine pts) = bimap f f (as,bs)
  where
    f = PolyLine . LSeq.forceLSeq (C  :: C 2)
    as = LSeq.take (i+1) pts
    bs = LSeq.drop i     pts

-- | Given a sequence of points, find the index of the point that has the
-- Furthest distance to the LineSegment. The result is the index of the point
-- and this distance.
maxDist       :: (Ord r, Fractional r, Arity d)
              => LSeq n (Point d r :+ p) -> LineSegment d p r -> (Int,r)
maxDist pts s = F.maximumBy (comparing snd) . LSeq.mapWithIndex (\i (p :+ _) ->
                                                     (i,sqDistanceToSeg p s)) $ pts
