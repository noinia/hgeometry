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

import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens hiding (contains)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Point.Orientation.Degenerate
import           Data.Geometry.Properties
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe
import Data.Function
import           Data.Ord (Down(..), comparing)
import qualified Data.Set as SS -- status struct
import qualified Data.Set.Util as SS -- status struct
import qualified Data.Set as EQ -- event queue
import           Data.Vinyl
import           Data.Vinyl.CoRec

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

-- -- | Compare based on the x-coordinate of the intersection with the horizontal
-- -- line through y
-- ordAt   :: (Fractional r, Ord r) => r -> Compare (LineSegment 2 p r)
-- ordAt y = comparing (xCoordAt y)

-- -- | Given a y coord and a line segment that intersects the horizontal line
-- -- through y, compute the x-coordinate of this intersection point.
-- --
-- -- note that we will pretend that the line segment is closed, even if it is not
-- xCoordAt             :: (Fractional r, Ord r) => r -> LineSegment 2 p r -> r
-- xCoordAt y (LineSegment' (Point2 px py :+ _) (Point2 qx qy :+ _))
--       | py == qy     = px `max` qx  -- s is horizontal, and since it by the
--                                     -- precondition it intersects the sweep
--                                     -- line, we return the x-coord of the
--                                     -- rightmost endpoint.
--       | otherwise    = px + alpha * (qx - px)
--   where
--     alpha = (y - py) / (qy - py)

--------------------------------------------------------------------------------
-- * The Main Sweep

type EventQueue      p r = EQ.Set (Event p r)
type StatusStructure p r = SS.Set (LineSegment 2 p r)

-- | Run the sweep handling all events
sweep       :: (Ord r, Num r)
            => [Event p r] -> StatusStructure p r -> Bool
sweep [] _ss = False
sweep (e:eq) ss = handle e eq ss

isClosedStart                     :: Eq r => Point 2 r -> LineSegment 2 p r -> Bool
isClosedStart p (LineSegment s e)
  | p == s^.unEndPoint.core       = isClosed s
  | otherwise                     = isClosed e

-- | Handle an event point
handle                           :: forall r p. (Ord r, Num r)
                                 => Event p r -> [Event p r] -> StatusStructure p r
                                 -> Bool
handle e@(eventPoint -> p) eq ss = overlaps || sweep eq ss'
  where
    starts                   = startSegs e
    (before,contains',after) = extractContains p ss
    (ends,contains)          = L.partition (endsAt p) contains'
    -- starting segments, exluding those that have an open starting point
    starts'  = filter (isClosedStart p) starts
    toReport = case starts' ++ contains' of
                 (_:_:_) -> [IntersectionPoint p $ associated (starts' <> ends) contains]
                 _       -> []
    
    overlaps 
      | F.null newSegs = fromMaybe False (segmentsOverlap <$> sl <*> sr)
      | otherwise      = let s'  = SS.lookupMin newSegs
                             s'' = SS.lookupMax newSegs
                        in or [ fromMaybe False (segmentsOverlap <$> sl <*> s')
                              , fromMaybe False (segmentsOverlap <$> s'' <*> sr)
                              ]

    -- new status structure
    ss' = before `SS.join` newSegs `SS.join` after
    newSegs = toStatusStruct p $ starts ++ contains

    -- -- the new eeventqueue
    -- eq' = foldr EQ.insert eq es
    -- -- the new events:
    -- es | F.null newSegs  = maybeToList $ app (findNewEvent p) sl sr
    --    | otherwise       = let s'  = SS.lookupMin newSegs
    --                            s'' = SS.lookupMax newSegs
    --                        in catMaybes [ app (findNewEvent p) sl  s'
    --                                     , app (findNewEvent p) s'' sr
    --                                     ]
    sl = SS.lookupMax before
    sr = SS.lookupMin after

    app f x y = do { x' <- x ; y' <- y ; f x' y'}

-- | split the status structure, extracting the segments that contain p.
-- the result is (before,contains,after)
extractContains      :: (Num r, Ord r)
                     => Point 2 r -> StatusStructure p r
                     -> (StatusStructure p r, [LineSegment 2 p r], StatusStructure p r)
extractContains p ss = (before, F.toList mid1 <> F.toList mid2, after)
  where
    -- (before, mid1, after') = SS.splitOn (xCoordAt $ p^.yCoord) (p^.xCoord) ss
    (before, mid1, after') = SS.splitBy lineOrder ss
    lineOrder line =
      case ccw (line^.end.core) (line^.start.core) p of
        CW       -> LT
        CoLinear -> EQ
        CCW      -> GT
    -- Make sure to also select the horizontal segments containing p
    (mid2, after) = SS.spanAntitone (\s -> p `onSegment2` s) after'


-- | Given a point and the linesegements that contain it. Create a piece of
-- status structure for it.
toStatusStruct      :: (Num r, Ord r)
                    => Point 2 r -> [LineSegment 2 p r] -> StatusStructure p r
toStatusStruct p xs = ss `SS.join` hors
  -- ss { SS.nav = ordAtNav $ p^.yCoord } `SS.join` hors
  where
    (hors',rest) = L.partition isHorizontal xs
    -- ss           = SS.fromListBy (ordAt $ maxY xs) rest
    ss = SS.fromListBy (ccwCmpAround p `on` view (end.core)) rest
    hors         = SS.fromListBy (comparing rightEndpoint) hors'

    isHorizontal s  = s^.start.core.yCoord == s^.end.core.yCoord

    -- find the y coord of the first interesting thing below the sweep at y
    -- maxY = maximum . filter (< p^.yCoord)
    --      . concatMap (\s -> [s^.start.core.yCoord,s^.end.core.yCoord])

-- | Get the right endpoint of a segment
rightEndpoint   :: Ord r => LineSegment 2 p r -> r
rightEndpoint s = (s^.start.core.xCoord) `max` (s^.end.core.xCoord)

-- | Test if a segment ends at p
endsAt                      :: Ord r => Point 2 r -> LineSegment 2 p r -> Bool
endsAt p (LineSegment' a b) = all (\q -> ordPoints (q^.core) p /= GT) [a,b]

--------------------------------------------------------------------------------
-- * Finding New events

-- | Find all events
findNewEvent       :: (Ord r, Fractional r)
                   => Point 2 r -> LineSegment 2 p r -> LineSegment 2 p r
                   -> Maybe (Event p r)
findNewEvent p l r = match (l `intersect` r) $
     H (const Nothing) -- NoIntersection
  :& H (\q -> if ordPoints q p == GT then Just (Event q Intersection)
                                     else Nothing)
  :& H (const Nothing) -- full segment intersectsions are handled
                       -- at insertion time
  :& RNil

-- Both segments are pointing downwards or are horizontal.
segmentsOverlap :: (Num r, Ord r) => LineSegment 2 p r -> LineSegment 2 p r -> Bool
segmentsOverlap a b = undefined
  -- ccw' (a^.start) (b^.start) (a^.end) /= ccw' (a^.start) (b^.end) (a^.end) &&
  -- ccwCmpAround
  -- ccw' (a^.start) (b^.start) (a^.end) /= ccw' (a^.start) (b^.end) (a^.end) &&

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