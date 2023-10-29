--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Intersection.BentleyOttmann
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The \(O((n+k)\log n)\) time line segment intersection algorithm by Bentley
-- and Ottmann.
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Intersection.BentleyOttmann
  ( intersections
  , interiorIntersections
  ) where

import           Control.Lens hiding (contains)
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Ord (Down(..), comparing)
import qualified Data.Set as EQ -- event queue
import qualified Data.Set as SS -- status struct
import qualified Data.Set as Set
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.LineSegment.Intersection.Types
import           HGeometry.Point
import           HGeometry.Properties
import qualified HGeometry.Set.Util as SS -- status struct

--------------------------------------------------------------------------------

-- | Compute all intersections
--
-- \(O((n+k)\log n)\), where \(k\) is the number of intersections.
intersections    ::  ( LineSegment_ lineSegment point
                     , Point_ point 2 r
                     , Eq lineSegment
                     , Ord r, Fractional r
                     , IsIntersectableWith lineSegment lineSegment
                     , OrdArounds lineSegment
                     , Intersection lineSegment lineSegment ~
                       Maybe (LineSegmentLineSegmentIntersection lineSegment)
                     )
                 => [lineSegment] -> Intersections r lineSegment
intersections ss = fmap unflipSegs . merge $ sweep pts SS.empty
  where
    pts = EQ.fromAscList . groupStarts . List.sort . concatMap (asEventPts . tagFlipped) $ ss

-- | Computes all intersection points p s.t. p lies in the interior of at least
-- one of the segments.
--
--  \(O((n+k)\log n)\), where \(k\) is the number of intersections.
interiorIntersections :: ( LineSegment_ lineSegment point
                         , Point_ point 2 r
                         , Eq lineSegment
                         , Ord r, Fractional r
                         , IsIntersectableWith lineSegment lineSegment
                         , OrdArounds lineSegment
                         , Intersection lineSegment lineSegment ~
                           Maybe (LineSegmentLineSegmentIntersection lineSegment)
                         )
                      => [lineSegment] -> Intersections r lineSegment
interiorIntersections = Map.filter isInteriorIntersection . intersections

--------------------------------------------------------------------------------
-- * Flipping and unflipping

data Flipped = NotFlipped | Flipped deriving (Show,Eq)

-- | Make sure the 'start' endpoint occurs before the end-endpoints in
-- terms of the sweep order.
tagFlipped   :: Ord r => lineSegment -> lineSegment :+ Flipped
tagFlipped s = case (s^.start) `ordPoints` (s^.end) of
                 GT -> s %~ flipSeg :+ Flipped
                 _  -> s            :+ NotFlipped

-- | Flips the segment
flipSeg     :: LineSegment_ lineSegment point => lineSegment -> lineSegment
flipSeg seg = seg&start .~ (seg^.end)
                 &end   .~ (seg^.start)

-- | Unflips the segments in an associated.
unflipSegs                       :: (Fractional r, Ord r)
                                 => Associated (lineSegment :+ Flipped)
                                 -> Associated lineSegment
unflipSegs = undefined
{-
(Associated ss es is) =
    Associated (dropFlipped ss1 <> unflipSegs' es')
               (dropFlipped es1 <> unflipSegs' ss')
               (dropFlipped is1 <> unflipSegs' is')
  where
    (ss',ss1) = Set.partition (\(AroundEnd          s) -> isFlipped s) ss
    (es',es1) = Set.partition (\(AroundStart        s) -> isFlipped s) es
    (is',is1) = Set.partition (\(AroundIntersection s) -> isFlipped s) is

    isFlipped s = Flipped == s^.extra.extra

    -- | For segments that are not acutally flipped, we can just drop the flipped bit
    dropFlipped :: Functor f
                => Set.Set (f (lineSegment :+ Flipped))
                -> Set.Set (f (lineSegment))
    dropFlipped = Set.mapMonotonic (fmap dropFlip)

    -- For flipped segs we unflip them (and appropriately coerce the
    -- so that they remain in the same order. I.e. if they were sorted
    -- around the start point they are now sorted around the endpoint.
    unflipSegs' :: ( Functor f
                   , Coercible (f (lineSegment)) (g (lineSegment))
                   )
                => Set.Set (f (lineSegment :+ Flipped))
                -> Set.Set (g (lineSegment))
    unflipSegs' = Set.mapMonotonic (coerce . fmap unflip)

    unflip   (s :+ (e :+ _)) = flipSeg s :+ e
    dropFlip (s :+ (e :+ _)) = s :+ e
-}
--------------------------------------------------------------------------------

-- | Computes the event points for a given line segment
asEventPts   :: lineSegment -> [Event r lineSegment]
asEventPts s = [ Event (s^.core.start.core) (Start $ s :| [])
               , Event (s^.core.end.core)   (End s)
               ]

-- | Group the segments with the intersection points
merge :: (Ord r, Fractional r)
      => [IntersectionPoint (Point 2 r) lineSegment] -> Intersections r lineSegment
merge = foldMap (\ip -> Map.singleton (ip^.intersectionPoint) (ip^.associatedSegs))

-- | Group the startpoints such that segments with the same start point
-- correspond to one event.
groupStarts                          :: Eq r => [Event r lineSegment] -> [Event r lineSegment]
groupStarts []                       = []
groupStarts (Event p (Start s) : es) = Event p (Start ss) : groupStarts rest
  where
    (ss',rest) = List.span sameStart es
    -- FIXME: this seems to keep the segments on decreasing y, increasing x. shouldn't we
    -- sort them cyclically around p instead?
    ss         = let (x:|xs) = s
                 in x :| (xs ++ concatMap startSegs ss')

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
data Event r lineSegment = Event { eventPoint :: !(Point 2 r)
                                 , eventType  :: !(EventType lineSegment)
                                 } deriving (Show,Eq)

instance Ord r => Ord (Event r lineSegment) where
  -- decreasing on the y-coord, then increasing on x-coord, and increasing on event-type
  (Event p s) `compare` (Event q t) = case ordPoints p q of
                                        EQ -> s `compare` t
                                        x  -> x

-- | Get the segments that start at the given event point
startSegs   :: Event r lineSegment -> [lineSegment]
startSegs e = case eventType e of
                Start ss -> NonEmpty.toList ss
                _        -> []

--------------------------------------------------------------------------------
-- * The Main Sweep

type EventQueue      r lineSegment = EQ.Set (Event r lineSegment)
type StatusStructure r lineSegment = SS.Set lineSegment

-- | Run the sweep handling all events
sweep       :: (Ord r, Fractional r)
            => EventQueue r lineSegment -> StatusStructure r lineSegment
            -> [IntersectionPoint (Point 2 r) lineSegment]
sweep eq ss = case EQ.minView eq of
    Nothing      -> []
    Just (e,eq') -> handle e eq' ss

isOpen :: EndPoint_ endPoint => endPoint -> Bool
isOpen = (== Open) . endPointType

isClosed :: EndPoint_ endPoint => endPoint -> Bool
isClosed = (== Open) . endPointType

-- | Handle an event point
handle                           :: ( LineSegment_ lineSegment point
                                    , Point_ point 2 r
                                    , Ord r, Fractional r
                                    )
                                 => Event r lineSegment
                                 -> EventQueue r lineSegment -> StatusStructure r lineSegment
                                 -> [IntersectionPoint (Point 2 r) lineSegment]
handle e@(eventPoint -> p) eq ss = toReport <> sweep eq' ss'
  where
    starts                   = startSegs e
    (before,contains',after) = extractContains p ss
    (ends,contains)          = List.partition (endsAt p) contains'
    -- starting segments, exluding those that have an open starting point
    -- starts' = filter (isClosedStart p) starts
    starts' = shouldReport p $ SS.toAscList newSegs

    -- If we just inserted open-ended segments that start here, then
    -- don't consider them to be "contained" segments.
    pureContains =
      filter (\seg -> not $ isOpen (seg^.startPoint) && p == s^.start.asPoint) contains

    -- any (closed) ending segments at this event point.
    closedEnds = filter (isClosed . view endPoint) ends

    toReport = case starts' <> closedEnds <> pureContains of
                 (_:_:_) -> [mkIntersectionPoint p (starts' <> closedEnds) pureContains]
                 _       -> []

    -- new status structure
    ss' = before `SS.join` newSegs `SS.join` after
    newSegs = toStatusStruct p $ starts ++ contains

    -- the new eeventqueue
    eq' = foldr EQ.insert eq es
    -- the new events:
    es | F.null newSegs  = maybeToList $ app (findNewEvent p) sl sr
       | otherwise       = let s'  = SS.lookupMin newSegs
                               s'' = SS.lookupMax newSegs
                           in catMaybes [ app (findNewEvent p) sl  s'
                                        , app (findNewEvent p) s'' sr
                                        ]
    sl = SS.lookupMax before
    sr = SS.lookupMin after

    app f x y = do { x' <- x ; y' <- y ; f x' y'}

-- | Given the starting point p, and the segments that either start in
-- p, or continue in p, in left to right order along a line just
-- epsilon below p, figure out which segments we should report as
-- intersecting at p.
--
-- in partcular; those that:
-- - have a closed endpoint at p
-- - those that have an open endpoint at p and have an intersection
--   with a segment eps below p. Those segments thus overlap wtih
--   their predecessor or successor in the cyclic order.
shouldReport   :: ( LineSegment_ lineSegment point
                  , Point_ point 2 r
                  , Ord r, Num r
                  ) => a -> [lineSegment] -> [lineSegment]
shouldReport _ = overlapsOr (isClosed . view startPoint) intersects

-- | split the status structure, extracting the segments that contain p.
-- the result is (before,contains,after)
extractContains      :: (Fractional r, Ord r)
                     => Point 2 r -> StatusStructure r lineSegment
                     -> (StatusStructure r lineSegment, [lineSegment], StatusStructure r lineSegment)
extractContains p ss = (before, F.toList mid1 <> F.toList mid2, after)
  where
    (before, mid1, after') = SS.splitOn (xCoordAt' $ p^.yCoord) (p^.xCoord) ss
    -- Make sure to also select the horizontal segments containing p
    (mid2, after) = SS.spanAntitone (intersects p . view core) after'
    xCoordAt' y sa = xCoordAt y (sa^.core)

-- | Given a point and the linesegements that contain it. Create a piece of
-- status structure for it.
toStatusStruct      :: (Fractional r, Ord r)
                    => Point 2 r -> [lineSegment] -> StatusStructure r lineSegment
toStatusStruct p xs = ss `SS.join` hors
  -- ss { SS.nav = ordAtNav $ p^.yCoord } `SS.join` hors
  where
    (hors',rest) = List.partition isHorizontal xs
    ss           = SS.fromListBy (ordAtY $ maxY xs) rest
    hors         = SS.fromListBy (comparing rightEndpoint) hors'

    isHorizontal s  = s^.start.yCoord == s^.end.yCoord

    -- ordAtY' q sa sb = ordAtY q sa sb

    -- find the y coord of the first interesting thing below the sweep at y
    maxY segs = maximum [ y
                        | s <- segs
                        , y <- [s^.start.yCoord, s^.end.yCoord], y < p^.yCoord
                        ]
    -- fixme, why can't segs be empty

-- | Get the right endpoint of a segment
rightEndpoint   :: (LineSegment_ lineSegment point, Point_ point 2 r, Ord r) => lineSegment -> r
rightEndpoint s = (s^.start.xCoord) `max` (s^.end.xCoord)

-- | Test if a segment ends at p
endsAt       :: ( LineSegment_ lineSegment point
                , Point_ point 2 r
                , Eq r
                ) => Point 2 r -> lineSegment -> Bool
endsAt p seg = seg^.end.asPoint == p
  -- all (\q -> ordPoints (q^.core) p /= GT) [a,b]

--------------------------------------------------------------------------------
-- * Finding New events

-- | Find all events
findNewEvent       :: ( LineSegment_ lineSegment point
                      , Point_ point 2 r, Ord r, Fractional r
                      , Intersection lineSegment lineSegment ~
                              Maybe (LineSegmentLineSegmentIntersection lineSegment)
                      )
                   =>  Point 2 r -> lineSegment -> lineSegment
                   -> Maybe (Event r lineSegment)
findNewEvent p l r = l `intersect` r >>= \case
    LineSegment_x_LineSegment_Point q
      | ordPoints q p == GT                 -> Just (Event q Intersection)
      | otherwise                           -> Nothing
    LineSegment_x_LineSegment_LineSegment _ -> Nothing
    -- full segment intersectsions are handled at insertion time

-- type R = Rational

-- seg1, seg2 :: LineSegment 2 () R
-- seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
-- seg2 = ClosedLineSegment (ext $ Point2 0 1) (ext $ Point2 0 5)


--------------------------------------------------------------------------------
-- *

-- | Given a predicate p on elements, and a predicate q on
-- (neighbouring) pairs of elements, filter the elements that satisfy
-- p, or together with one of their neighbours satisfy q.
overlapsOr     :: (a -> Bool)
               -> (a -> a -> Bool)
               -> [a]
               -> [a]
overlapsOr p q = map fst . filter snd . map (\((a,b),b') -> (a, b || b'))
               . overlapsWithNeighbour (q `on` fst)
               . map (\x -> (x, p x))

-- | Given a predicate, test and a list, annotate each element whether
-- it, together with one of its neighbors satisifies the predicate.
overlapsWithNeighbour   :: (a -> a -> Bool) -> [a] -> [(a,Bool)]
overlapsWithNeighbour p = go0
  where
    go0 = \case
      []     -> []
      (x:xs) -> go x False xs

    go x b = \case
      []     -> []
      (y:ys) -> let b' = p x y
                in (x,b || b') : go y b' ys

-- annotateReport   :: (a -> Bool) -> [a] -> [(a,Bool)]
-- annotateReport p = map (\x -> (x, p x))

overlapsWithNext'   :: (a -> a -> Bool) -> [a] -> [(a,Bool)]
overlapsWithNext' p = go
  where
    go = \case
      []           -> []
      [x]          -> [(x,False)]
      (x:xs@(y:_)) -> (x,p x y) : go xs

overlapsWithPrev'   :: (a -> a -> Bool) -> [a] -> [(a,Bool)]
overlapsWithPrev' p = go0
  where
    go0 = \case
      []     -> []
      (x:xs) -> (x,False) : go x xs

    go x = \case
      []     -> []
      (y:ys) -> (y,p x y) : go y ys


overlapsWithNeighbour2 p = map (\((a,b),b') -> (a, b || b'))
                         . overlapsWithNext' (p `on` fst)
                         . overlapsWithPrev' p

shouldBe :: Eq a => a -> a -> Bool
shouldBe = (==)

propSameAsSeparate p xs = overlapsWithNeighbour p xs `shouldBe` overlapsWithNeighbour2 p xs

test' = overlapsWithNeighbour (==) testOverlapNext
testOverlapNext = [1,2,3,3,3,5,6,6,8,10,11,34,2,2,3]
