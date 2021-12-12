{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection.BooleanSweep
-- Copyright   :  (C) Frank Staals, David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- \( O(n \log n) \) algorithm for determining if any two sets of line segments intersect.
--
-- Shamos and Hoey.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweep
  ( hasIntersections
  ) where

import           Control.Lens hiding (contains)
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point

import           Data.Intersection
import qualified Data.List as L
import           Data.Maybe
import           Data.Ord (Down (..), comparing)
import qualified Data.Set as SS
import qualified Data.Set.Util as SS

-- import           Data.RealNumber.Rational
import Debug.Trace
import Data.Geometry.Polygon

--------------------------------------------------------------------------------

-- | Tests if there are any intersections.
--
-- \(O(n\log n)\)
hasIntersections    :: (Ord r, Num r, Show r, Show p)
                 => [LineSegment 2 p r] -> Bool
hasIntersections ss = sweep pts SS.empty
  where
    pts = tr "events" $ L.sortBy ordEvents . concatMap asEventPts $ ss

-- | Computes the event points for a given line segment
asEventPts   :: Ord r => LineSegment 2 p r -> [Event p r]
asEventPts s =
  case ordPoints (s^.start.core) (s^.end.core) of
    LT -> [Insert s, Delete s]
    _  -> let LineSegment a b = s
              s' = LineSegment b a
          in [Insert s', Delete s']

--------------------------------------------------------------------------------
-- * Data type for Events

-- | The actual event consists of a point and its type
data Event p r = Insert (LineSegment 2 p r) | Delete (LineSegment 2 p r)
               deriving (Show)

eventPoint :: Event p r -> Point 2 r
eventPoint (Insert l) = l^.start.core
eventPoint (Delete l) = l^.end.core

-- Sort order:
--  1. Y-coord. Larger Ys before smaller.
--  2. X-coord. Smaller Xs before larger.
--  3. Type: Inserts before deletions
ordEvents :: (Num r, Ord r) => Event p r -> Event p r -> Ordering
ordEvents e1 e2 = ordPoints (eventPoint e1) (eventPoint e2) <> cmpType e1 e2
  where
    cmpType Insert{} Delete{} = LT
    cmpType Delete{} Insert{} = GT
    cmpType _ _               = EQ

-- | An ordering that is decreasing on y, increasing on x
ordPoints     :: Ord r => Point 2 r -> Point 2 r -> Ordering
ordPoints a b = let f p = (Down $ p^.yCoord, p^.xCoord) in comparing f a b

--------------------------------------------------------------------------------
-- * The Main Sweep

type StatusStructure p r = SS.Set (LineSegment 2 p r)

-- | Run the sweep handling all events
sweep :: forall r p. (Ord r, Num r, Show r, Show p)
      => [Event p r] -> StatusStructure p r
      -> Bool
sweep [] _ = False
sweep (Delete l:eq) ss =
    overlaps || sweep eq ss'
  where
    p = l^.end.core
    (before,_contains,after) = splitBeforeAfter p ss
    overlaps = fromMaybe False (intersects <$> sl <*> sr)
    sl = SS.lookupMax before
    sr = SS.lookupMin after
    ss' = before `SS.join` after
sweep (Insert l@(LineSegment startPoint _endPoint):eq) ss = tr ("insert " <> show l) $
    endOverlap || overlaps || sweep eq ss'
  where
    p = l^.start.core
    (before,contains,after) = splitBeforeAfter p ss

    -- Check whether the endpoint is contained in one of the existing
    -- segments. The only segments that could qualify are the ones in
    -- 'contains'. Hence check only those. Note that it is not
    -- sufficient just to check whether 'contains' is empty or not,
    -- since there may be segments whose endpoint is open and coincides with p.
    endOverlap = isClosed startPoint && any (p `intersects`) contains

    overlaps = tr "overlaps" $
      or [ fromMaybe False (tr ("left:" <> show (l,sl)) $ intersects l <$> sl)
                  , fromMaybe False (tr ("right: " <> show (l,sr)) $ intersects l <$> sr) ]
    sl = SS.lookupMax before
    sr = SS.lookupMin after
    ss' = before `SS.join` SS.singleton l `SS.join` after


-- | split the status structure around p.
-- the result is (before,contains,after)
splitBeforeAfter      :: (Num r, Ord r)
                     => Point 2 r -> StatusStructure p r
                     -> (StatusStructure p r, [LineSegment 2 p r],StatusStructure p r)
splitBeforeAfter p ss = (before, filter (not . endsAt p) $ SS.toList contains, after)
  where
    (before,contains,after) = SS.splitBy cmpLine ss
    cmpLine line
      | isHorizontal line =
        let [_top,bot] = L.sortBy ordPoints [line^.start.core,line^.end.core] in
        (bot^.xCoord) `compare` (p^.xCoord)
    cmpLine line =
      let [top,bot] = L.sortBy ordPoints [line^.start.core,line^.end.core] in
      case ccw bot top p of
        CW       -> LT
        CoLinear -> EQ
        CCW      -> GT


isHorizontal :: Eq r => LineSegment 2 p r -> Bool
isHorizontal s  = s^.start.core.yCoord == s^.end.core.yCoord

-- | Test if a segment ends at p
endsAt                     :: Ord r => Point 2 r -> LineSegment 2 p r -> Bool
endsAt p (LineSegment _ b) = fmap (view core) b == Open p

--------------------------------------------------------------------------------
-- * Finding New events

-- -- | Given two segments test if they intersect. Why don't we simply use 'intersect'
-- segmentsOverlap :: (Num r, Ord r) => LineSegment 2 p r -> LineSegment 2 p r -> Bool
-- segmentsOverlap a@(LineSegment aStart aEnd) b =
--     (isClosed aStart && (aStart^.unEndPoint.core) `intersects` b) ||
--     (isClosed aEnd && (aEnd^.unEndPoint.core) `intersects` b) ||
--     (opposite (ccw' (a^.start) (b^.start) (a^.end)) (ccw' (a^.start) (b^.end) (a^.end)) &&
--     not (onTriangleRelaxed (a^.end.core) t1) &&
--     not (onTriangleRelaxed (a^.start.core) t2))
--   where
--     opposite CW CCW = True
--     opposite CCW CW = True
--     opposite _ _    = False
--     t1 = Triangle (a^.start) (b^.start) (b^.end)
--     t2 = Triangle (a^.end) (b^.start) (b^.end)


bug' = hasIntersections $ listEdges bug

bug :: SimplePolygon () Int
bug = fromPoints . map ext $ [
  Point2 144 592
  , Point2 336 624
  , Point2 320 544
  , Point2 240 624
  ]

s1, s2 :: LineSegment 2 () Int
s1 = read "LineSegment (Closed (Point2 240 620 :+ ())) (Open (Point2 320 544 :+ ()))"
s2 = read "LineSegment (Closed (Point2 144 592 :+ ())) (Open (Point2 336 624 :+ ()))"

tr s x = traceShow (s <> " : ", x) x

edges' :: [LineSegment 2 () Int]
edges' = [ LineSegment (Closed (Point2 240 624 :+ ())) (Open (Point2 320 544 :+ ()))
--         , LineSegment (Closed (Point2 320 544 :+ ())) (Open (Point2 336 624 :+ ()))
         , LineSegment (Closed (Point2 336 624 :+ ())) (Open (Point2 144 592 :+ ()))
         , LineSegment (Closed (Point2 144 592 :+ ())) (Open (Point2 240 624 :+ ()))
         ]

-- ah, I guess it selects the wrong predecessor/successor seg, since they overlap at the endpoint.
