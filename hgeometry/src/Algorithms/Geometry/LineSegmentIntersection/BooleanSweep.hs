{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection.BooleanSweep
-- Copyright   :  (C) Frank Staals, David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- The \(O((n+k)\log n)\) time line segment intersection algorithm by Bentley
-- and Ottmann.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweep
  ( intersections
  , segmentsOverlap
  ) where

import           Control.Lens              hiding (contains)
import           Data.Ext
import qualified Data.Foldable             as F
import           Data.Function
import           Data.Geometry.Interval
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Geometry.Triangle
import qualified Data.List                 as L
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Maybe
import           Data.Ord                  (Down (..), comparing)
import qualified Data.Set                  as SS
import qualified Data.Set.Util             as SS
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import Data.RealNumber.Rational

import Debug.Trace

{-
Scan line is horizontal and moves from +y to -y.
-}

type R = RealNumber 5
line1, line2, line3, line4, line5, line6, line7, line8, line9 :: LineSegment 2 () R

line1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 1 0)
line2 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 1)

line3 = OpenLineSegment (ext $ Point2 0 0) (ext $ Point2 1 0)
line4 = OpenLineSegment (ext $ Point2 0 0) (ext $ Point2 0 1)

line5 = ClosedLineSegment (ext $ Point2 1 0) (ext $ Point2 2 0)
line6 = OpenLineSegment (ext $ Point2 1 0) (ext $ Point2 2 0)

line7 = LineSegment (Closed (Point2 0.1 0.2 :+ ())) (Closed (Point2 0.1 4.2 :+ ()))
line8 = LineSegment (Open (Point2 7.2 1.2 :+ ())) (Closed (Point2 0.9 (-4.9) :+ ()))
line9 = LineSegment (Closed (Point2 (-5.1) 0.9 :+ ())) (Closed (Point2 2.9 (-3.8) :+ ()))
{-

[LineSegment (Closed (Point2 0.1303 0.22712 :+ ())) (Closed (Point2 0.16269 4.28771 :+ ()))
,LineSegment (Open (Point2 7.27324 1.2707 :+ ())) (Closed (Point2 0.96801 (-4.98436) :+ ()))
,LineSegment (Closed (Point2 (-5.16588) 0.96468 :+ ())) (Closed (Point2 2.90372 (-3.84283) :+ ()))]
-}

--------------------------------------------------------------------------------

-- | Compute all intersections
--
-- \(O(n\log n)\)
intersections    :: (Ord r, Num r, _)
                 => [LineSegment 2 p r] -> Bool
intersections ss = sweep pts SS.empty
  where
    pts = L.sort . concatMap asEventPts $ ss

-- -- | Computes all intersection points p s.t. p lies in the interior of at least
-- -- one of the segments.
-- --
-- --  \(O((n+k)\log n)\), where \(k\) is the number of intersections.
-- interiorIntersections :: (Ord r, Fractional r)
--                        => [LineSegment 2 p r] -> Intersections p r
-- interiorIntersections = M.filter (not . isEndPointIntersection) . intersections

-- | Computes the event points for a given line segment
asEventPts   :: Ord r => LineSegment 2 p r -> [Event p r]
asEventPts s =
  case ordPoints (s^.start.core) (s^.end.core) of
    LT -> [Insert s, Delete s]
    _  -> let LineSegment a b = s
              s' = LineSegment b a
          in [Insert s', Delete s']

-- -- | Group the segments with the intersection points
-- merge :: Ord r =>  [IntersectionPoint p r] -> Intersections p r
-- merge = foldr (\(IntersectionPoint p a) -> M.insertWith (<>) p a) M.empty

-- -- | Group the startpoints such that segments with the same start point
-- -- correspond to one event.
-- groupStarts                          :: Eq r => [Event p r] -> [Event p r]
-- groupStarts []                       = []
-- groupStarts (Event p (Start s) : es) = Event p (Start ss) : groupStarts rest
--   where
--     (ss',rest) = L.span sameStart es
--     -- sort the segs on lower endpoint
--     ss         = let (x:|xs) = s in x :| (xs ++ concatMap startSegs ss')

--     sameStart (Event q (Start _)) = p == q
--     sameStart _                   = False
-- groupStarts (e : es)                 = e : groupStarts es

--------------------------------------------------------------------------------
-- * Data type for Events

-- | The actual event consists of a point and its type
data Event p r = Insert (LineSegment 2 p r) | Delete (LineSegment 2 p r)

instance (Eq r) => Eq (Event p r) where
  e1 == e2 = (eventPoint e1, eventOtherPoint e1) == (eventPoint e2, eventOtherPoint e2)

eventPoint :: Event p r -> Point 2 r
eventPoint (Insert l) = l^.start.core
eventPoint (Delete l) = l^.end.core

eventSegment :: Event p r -> LineSegment 2 p r
eventSegment (Insert l) = l
eventSegment (Delete l) = l

eventOtherPoint :: Event p r -> Point 2 r
eventOtherPoint (Insert l) = l^.end.core
eventOtherPoint (Delete l) = l^.start.core

-- Sort order:
--  1. Y-coord. Larger Ys before smaller.
--  2. X-coord. Smaller Xs before larger.
--  3. Type: Inserts before deletions
--  4. Counter-clockwise by opposite endpoints.
--  5. X-coord of opposite endpoints.
instance (Num r, Ord r) => Ord (Event p r) where
  e1 `compare` e2 =
    ordPoints (eventPoint e1) (eventPoint e2) <>
    cmpType e1 e2 -- <>
    -- ccwCmpAroundWith (Vector2 0 1) (eventPoint e1) (eventOtherPoint e1) (eventOtherPoint e2) <>
    -- compare (eventOtherPoint e1 ^. xCoord) (eventOtherPoint e2 ^. xCoord)
    where
      cmpType Insert{} Delete{} = LT
      cmpType Delete{} Insert{} = GT
      cmpType _ _ = EQ
  -- decreasing on the y-coord, then increasing on x-coord, and increasing on event-type
  -- (Event p s) `compare` (Event q t) = case ordPoints p q of
  --                                       EQ -> s `compare` t
  --                                       x  -> x

-- | An ordering that is decreasing on y, increasing on x
ordPoints     :: Ord r => Point 2 r -> Point 2 r -> Ordering
ordPoints a b = let f p = (Down $ p^.yCoord, p^.xCoord) in comparing f a b

-- -- | Get the segments that start at the given event point
-- startSegs   :: Event p r -> [LineSegment 2 p r]
-- startSegs e = case eventType e of
--                 Start ss -> NonEmpty.toList ss
--                 _        -> []

--------------------------------------------------------------------------------
-- * The Main Sweep

type StatusStructure p r = SS.Set (LineSegment 2 p r)

-- | Run the sweep handling all events
sweep                           :: forall r p. (Ord r, Num r, _)
                                 => [Event p r] -> StatusStructure p r
                                 -> Bool
sweep [] _ = False
sweep (Delete l:eq) ss =
    overlaps || sweep eq ss'
  where
    p = l^.end.core
    (before,contains',after) = splitBeforeAfter p ss
    overlaps = fromMaybe False (segmentsOverlap <$> sl <*> sr)
    sl = SS.lookupMax before
    sr = SS.lookupMin after
    ss' = before `SS.join` after
sweep (Insert l@(LineSegment startPoint endPoint):eq) ss =
    endOverlap || overlaps || sweep eq ss'
  where
    p = l^.start.core
    (before,contains,after) = splitBeforeAfter p ss
    endOverlap =
      (not (null contains) && isClosed startPoint)
    -- hasAtLeast lst n = not (null (drop (n-1) lst))
    overlaps = or [ fromMaybe False (segmentsOverlap l <$> sl)
                  , fromMaybe False (segmentsOverlap l <$> sr) ]
    sl = SS.lookupMax before
    sr = SS.lookupMin after
    ss' = before `SS.join` SS.singleton l `SS.join` after
-- sweep (e@(eventPoint -> p):eq) ss = traceShow (e,before,after) $ overlaps || sweep eq ss'
--   where
--     starts         = startSegs e
--     (before,contains',after) = splitBeforeAfter p ss
--     (ends,contains)          = L.partition (endsAt p) contains'

--     overlaps
--       | F.null newSegs = fromMaybe False (segmentsOverlap <$> sl <*> sr)
--       | otherwise      = let s'  = SS.lookupMin newSegs
--                              s'' = SS.lookupMax newSegs
--                         in or [ fromMaybe False (segmentsOverlap <$> sl <*> s')
--                               , fromMaybe False (segmentsOverlap <$> s'' <*> sr)
--                               ]

--     -- new status structure
--     ss' = before `SS.join` newSegs `SS.join` after
--     newSegs = toStatusStruct p starts

--     sl = SS.lookupMax before
--     sr = SS.lookupMin after

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
        let [top,bot] = L.sortBy ordPoints [line^.start.core,line^.end.core] in
        (bot^.xCoord) `compare` (p^.xCoord)
    cmpLine line =
      let [top,bot] = L.sortBy ordPoints [line^.start.core,line^.end.core] in
      case ccw bot top p of
        CW       -> LT
        CoLinear -> EQ
        CCW      -> GT


-- | Given a point and the linesegements that contain it. Create a piece of
-- status structure for it.
toStatusStruct      :: (Num r, Ord r)
                    => Point 2 r -> [LineSegment 2 p r] -> StatusStructure p r
toStatusStruct p xs = ss `SS.join` hors
  where
    (hors',rest) = L.partition isHorizontal xs
    ss           = SS.fromListBy (ccwCmpAround p `on` view (end.core)) rest
    hors         = SS.fromListBy (comparing rightEndpoint) hors'

isHorizontal :: Eq r => LineSegment 2 p r -> Bool
isHorizontal s  = s^.start.core.yCoord == s^.end.core.yCoord

-- | Get the right endpoint of a segment
rightEndpoint   :: Ord r => LineSegment 2 p r -> r
rightEndpoint s = (s^.start.core.xCoord) `max` (s^.end.core.xCoord)

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
    opposite _ _ = False
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
