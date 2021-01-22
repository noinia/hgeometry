{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection.BooleanSweep
-- Copyright   :  (C) Frank Staals, David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- \( O(n \log n) \) algorithm for determining if any two line segments overlap.
--
-- Shamos and Hoey.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweep
  ( hasIntersections
  , segmentsOverlap
  ) where

import           Control.Lens              hiding (contains)
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import qualified Data.List                 as L
import           Data.Maybe
import           Data.Ord                  (Down (..), comparing)
import qualified Data.Set                  as SS
import qualified Data.Set.Util             as SS

-- import           Data.RealNumber.Rational
-- import Debug.Trace

--------------------------------------------------------------------------------

-- | Tests if there are any intersections.
--
-- \(O(n\log n)\)
hasIntersections    :: (Ord r, Num r)
                 => [LineSegment 2 p r] -> Bool
hasIntersections ss = sweep pts SS.empty
  where
    pts = L.sortBy ordEvents . concatMap asEventPts $ ss

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
sweep :: forall r p. (Ord r, Num r)
      => [Event p r] -> StatusStructure p r
      -> Bool
sweep [] _ = False
sweep (Delete l:eq) ss =
    overlaps || sweep eq ss'
  where
    p = l^.end.core
    (before,_contains,after) = splitBeforeAfter p ss
    overlaps = fromMaybe False (segmentsOverlap <$> sl <*> sr)
    sl = SS.lookupMax before
    sr = SS.lookupMin after
    ss' = before `SS.join` after
sweep (Insert l@(LineSegment startPoint _endPoint):eq) ss =
    endOverlap || overlaps || sweep eq ss'
  where
    p = l^.start.core
    (before,contains,after) = splitBeforeAfter p ss
    endOverlap =
      (not (null contains) && isClosed startPoint)
    overlaps = or [ fromMaybe False (segmentsOverlap l <$> sl)
                  , fromMaybe False (segmentsOverlap l <$> sr) ]
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

segmentsOverlap :: (Num r, Ord r) => LineSegment 2 p r -> LineSegment 2 p r -> Bool
segmentsOverlap a@(LineSegment aStart aEnd) b =
    (isClosed aStart && (aStart^.unEndPoint.core) `onSegment2` b) ||
    (isClosed aEnd && (aEnd^.unEndPoint.core) `onSegment2` b) ||
    (opposite (ccw' (a^.start) (b^.start) (a^.end)) (ccw' (a^.start) (b^.end) (a^.end)) &&
    not (onTriangleRelaxed (a^.end.core) t1) &&
    not (onTriangleRelaxed (a^.start.core) t2))
  where
    opposite CW CCW = True
    opposite CCW CW = True
    opposite _ _    = False
    t1 = Triangle (a^.start) (b^.start) (b^.end)
    t2 = Triangle (a^.end) (b^.start) (b^.end)

-- Copied from Data.Geometry.LineSegment.Internal. Delete when PR#62 is merged.
onSegment2                          :: (Ord r, Num r)
                                    => Point 2 r -> LineSegment 2 p r -> Bool
p `onSegment2` s@(LineSegment u v) = case ccw' (ext p) (u^.unEndPoint) (v^.unEndPoint) of
    CoLinear -> let su = p `onSide` lu
                    sv = p `onSide` lv
                in su /= sv
                && ((su == OnLine) `implies` isClosed u)
                && ((sv == OnLine) `implies` isClosed v)
    _        -> False
  where
    (Line _ w) = perpendicularTo $ supportingLine s
    lu = Line (u^.unEndPoint.core) w
    lv = Line (v^.unEndPoint.core) w

    a `implies` b = b || not a
