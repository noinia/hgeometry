module Algorithms.Geometry.PolyLineSimplification.DouglasPeucker where

import Data.Semigroup
import Data.Ord(comparing)
import Control.Lens
import Data.Ext
import Data.Geometry.PolyLine
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.LineSegment
import qualified Data.Seq2 as S2
import qualified Data.Sequence as S
import qualified Data.Foldable as F



douglasPeucker         :: (Ord r, Fractional r, Arity d)
                       => r -> PolyLine d p r -> PolyLine d p r
douglasPeucker eps pl
    | dst <= (eps*eps) = fromPoints [a,b]
    | otherwise        = douglasPeucker eps pref `merge` douglasPeucker eps subf
  where
    pts@(S2.Seq2 a _ b) = pl^.points
    (i,dst)             = maxDist pts (ClosedLineSegment a b)

    (pref,subf)         = split i pl

merge          :: PolyLine d p r -> PolyLine d p r -> PolyLine d p r
merge pref sub = PolyLine $ pref' >+< (sub^.points)
  where
    (pref' S2.:>> _) = S2.viewr $ pref^.points
    ~(a S2.:< as) >+< bs = S2.fromSeqUnsafe $ a S.<| as <> S2.toSeq bs

split                  :: Int -> PolyLine d p r
                       -> (PolyLine d p r, PolyLine d p r)
split i (PolyLine pts) = bimap f f (as,bs)
  where
    f = PolyLine . S2.fromSeqUnsafe
    as = S2.take i pts
    bs = S2.drop (i-1) pts


maxDist       :: (Ord r, Fractional r, Arity d)
              => S2.Seq2 (Point d r :+ p) -> LineSegment d p r -> (Int,r)
maxDist pts s = F.maximumBy (comparing snd) . S2.mapWithIndex (\i (p :+ _) ->
                                                     (i,sqDistanceToSeg p s)) $ pts
