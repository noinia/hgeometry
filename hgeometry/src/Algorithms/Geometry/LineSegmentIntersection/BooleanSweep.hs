{-# LANGUAGE ScopedTypeVariables #-}
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
  ) where

import           Control.Lens hiding (contains)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import Data.Function
import           Data.Ord (Down(..), comparing)
import qualified Data.Set as SS -- status struct
import qualified Data.Set.Util as SS -- status struct

{-
Scan line is horizontal and moves from +y to -y.
-}

--------------------------------------------------------------------------------

-- | Compute all intersections
--
-- \(O(n\log n)\)
intersections    :: (Ord r, Num r)
                 => [LineSegment 2 p r] -> Bool
intersections ss = sweep pts SS.empty
  where
    pts = groupStarts . L.sort . concatMap asEventPts $ ss

-- -- | Computes all intersection points p s.t. p lies in the interior of at least
-- -- one of the segments.
-- --
-- --  \(O((n+k)\log n)\), where \(k\) is the number of intersections.
-- interiorIntersections :: (Ord r, Fractional r)
--                        => [LineSegment 2 p r] -> Intersections p r
-- interiorIntersections = M.filter (not . isEndPointIntersection) . intersections

-- | Computes the event points for a given line segment
asEventPts   :: Ord r => LineSegment 2 p r -> [Event p r]
asEventPts s = let [p,q] = L.sortBy ordPoints [s^.start.core,s^.end.core]
               in [Event p (Start $ s :| []), Event q (End s)]

-- -- | Group the segments with the intersection points
-- merge :: Ord r =>  [IntersectionPoint p r] -> Intersections p r
-- merge = foldr (\(IntersectionPoint p a) -> M.insertWith (<>) p a) M.empty

-- | Group the startpoints such that segments with the same start point
-- correspond to one event.
groupStarts                          :: Eq r => [Event p r] -> [Event p r]
groupStarts []                       = []
groupStarts (Event p (Start s) : es) = Event p (Start ss) : groupStarts rest
  where
    (ss',rest) = L.span sameStart es
    -- sort the segs on lower endpoint
    ss         = let (x:|xs) = s in x :| (xs ++ concatMap startSegs ss')

    sameStart (Event q (Start _)) = p == q
    sameStart _                   = False
groupStarts (e : es)                 = e : groupStarts es

--------------------------------------------------------------------------------
-- * Data type for Events

-- | Type of segment
data EventType s = Start !(NonEmpty s)| Intersection | End !s deriving (Show)

instance Eq (EventType s) where
  a == b = a `compare` b == EQ

instance Ord (EventType s) where
  (Start _)    `compare` (Start _)    = EQ
  (Start _)    `compare` _            = LT
  Intersection `compare` (Start _)    = GT
  Intersection `compare` Intersection = EQ
  Intersection `compare` (End _)      = LT
  (End _)      `compare` (End _)      = EQ
  (End _)      `compare` _            = GT

-- | The actual event consists of a point and its type
data Event p r = Event { eventPoint :: !(Point 2 r)
                       , eventType  :: !(EventType (LineSegment 2 p r))
                       } deriving (Show,Eq)

instance Ord r => Ord (Event p r) where
  -- decreasing on the y-coord, then increasing on x-coord, and increasing on event-type
  (Event p s) `compare` (Event q t) = case ordPoints p q of
                                        EQ -> s `compare` t
                                        x  -> x

-- | An ordering that is decreasing on y, increasing on x
ordPoints     :: Ord r => Point 2 r -> Point 2 r -> Ordering
ordPoints a b = let f p = (Down $ p^.yCoord, p^.xCoord) in comparing f a b

-- | Get the segments that start at the given event point
startSegs   :: Event p r -> [LineSegment 2 p r]
startSegs e = case eventType e of
                Start ss -> NonEmpty.toList ss
                _        -> []

--------------------------------------------------------------------------------
-- * The Main Sweep

type StatusStructure p r = SS.Set (LineSegment 2 p r)

-- | Run the sweep handling all events
sweep                           :: forall r p. (Ord r, Num r)
                                 => [Event p r] -> StatusStructure p r
                                 -> Bool
sweep [] _ = False
sweep (e@(eventPoint -> p):eq) ss = overlaps || sweep eq ss'
  where
    starts         = startSegs e
    (before,after) = splitBeforeAfter p ss
    
    overlaps 
      | F.null newSegs = fromMaybe False (segmentsOverlap <$> sl <*> sr)
      | otherwise      = let s'  = SS.lookupMin newSegs
                             s'' = SS.lookupMax newSegs
                        in or [ fromMaybe False (segmentsOverlap <$> sl <*> s')
                              , fromMaybe False (segmentsOverlap <$> s'' <*> sr)
                              ]

    -- new status structure
    ss' = before `SS.join` newSegs `SS.join` after
    newSegs = toStatusStruct p starts

    sl = SS.lookupMax before
    sr = SS.lookupMin after

-- | split the status structure around p.
-- the result is (before,after)
splitBeforeAfter      :: (Num r, Ord r)
                     => Point 2 r -> StatusStructure p r
                     -> (StatusStructure p r, StatusStructure p r)
splitBeforeAfter p ss = (before, after)
  where
    (before,after) = SS.spanAntitone lineIsBefore ss
    lineIsBefore line =
      case ccw (line^.end.core) (line^.start.core) p of
        CW       -> True
        CoLinear -> False
        CCW      -> False


-- | Given a point and the linesegements that contain it. Create a piece of
-- status structure for it.
toStatusStruct      :: (Num r, Ord r)
                    => Point 2 r -> [LineSegment 2 p r] -> StatusStructure p r
toStatusStruct p xs = ss `SS.join` hors
  where
    (hors',rest) = L.partition isHorizontal xs
    ss           = SS.fromListBy (ccwCmpAround p `on` view (end.core)) rest
    hors         = SS.fromListBy (comparing rightEndpoint) hors'

    isHorizontal s  = s^.start.core.yCoord == s^.end.core.yCoord

-- | Get the right endpoint of a segment
rightEndpoint   :: Ord r => LineSegment 2 p r -> r
rightEndpoint s = (s^.start.core.xCoord) `max` (s^.end.core.xCoord)

--------------------------------------------------------------------------------
-- * Finding New events

segmentsOverlap :: (Num r, Ord r) => LineSegment 2 p r -> LineSegment 2 p r -> Bool
segmentsOverlap a b =
    (a^.start.core) `onSegment2` b ||
    (a^.end.core) `onSegment2` b ||
    (ccw' (a^.start) (b^.start) (a^.end) /= ccw' (a^.start) (b^.end) (a^.end) &&
    not (onTriangleRelaxed (a^.end.core) t1) &&
    not (onTriangleRelaxed (a^.start.core) t2))
  where
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