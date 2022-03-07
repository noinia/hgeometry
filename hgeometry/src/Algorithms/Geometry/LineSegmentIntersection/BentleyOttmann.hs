{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The \(O((n+k)\log n)\) time line segment intersection algorithm by Bentley
-- and Ottmann.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann
  ( intersections
  , interiorIntersections
  ) where

import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens hiding (contains)
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord (Down(..), comparing)
import qualified Data.Set as EQ -- event queue
import qualified Data.Set as SS -- status struct
import qualified Data.Set as Set
import qualified Data.Set.Util as SS -- status struct
import           Data.Vinyl
import           Data.Vinyl.CoRec
--------------------------------------------------------------------------------

-- | Compute all intersections
--
-- \(O((n+k)\log n)\), where \(k\) is the number of intersections.
intersections    :: forall p r e. (Ord r, Fractional r)
                 => [LineSegment 2 p r :+ e] -> Intersections p r e
intersections ss = fmap unflipSegs . merge $ sweep pts SS.empty
  where
    pts = EQ.fromAscList . groupStarts . L.sort . concatMap (asEventPts . tagFlipped) $ ss

-- | Computes all intersection points p s.t. p lies in the interior of at least
-- one of the segments.
--
--  \(O((n+k)\log n)\), where \(k\) is the number of intersections.
interiorIntersections :: (Ord r, Fractional r)
                       => [LineSegment 2 p r :+ e] -> Intersections p r e
interiorIntersections = M.filter isInteriorIntersection . intersections

--------------------------------------------------------------------------------
-- * Flipping and unflipping

data Flipped = NotFlipped | Flipped deriving (Show,Eq)

-- | Make sure the 'start' endpoint occurs before the end-endpoints in
-- terms of the sweep order.
tagFlipped   :: Ord r => LineSegment 2 p r :+ e -> LineSegment 2 p r :+ (e :+ Flipped)
tagFlipped s = case (s^.core.start.core) `ordPoints` (s^.core.end.core) of
                 GT -> s&core  %~ flipSeg
                        &extra %~ (:+ Flipped)
                 _  -> s&extra %~ (:+ NotFlipped)

-- | Flips the segment
flipSeg     :: LineSegment d p r -> LineSegment d p r
flipSeg seg = seg&start .~ (seg^.end)
                 &end   .~ (seg^.start)

-- | Unflips the segments in an associated.
unflipSegs                       :: (Fractional r, Ord r)
                                 => Associated p r (e :+ Flipped) -> Associated p r e
unflipSegs (Associated ss es is) =
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
                => Set.Set (f (LineSegment 2 p r :+ (e :+ Flipped)))
                -> Set.Set (f (LineSegment 2 p r :+ e))
    dropFlipped = Set.mapMonotonic (fmap dropFlip)

    -- For flipped segs we unflip them (and appropriately coerce the
    -- so that they remain in the same order. I.e. if they were sorted
    -- around the start point they are now sorted around the endpoint.
    unflipSegs' :: ( Functor f
                   , Coercible (f (LineSegment 2 p r :+ e)) (g (LineSegment 2 p r :+ e))
                   )
                => Set.Set (f (LineSegment 2 p r :+ (e :+ Flipped)))
                -> Set.Set (g (LineSegment 2 p r :+ e))
    unflipSegs' = Set.mapMonotonic (coerce . fmap unflip)

    unflip   (s :+ (e :+ _)) = flipSeg s :+ e
    dropFlip (s :+ (e :+ _)) = s :+ e

--------------------------------------------------------------------------------

-- | Computes the event points for a given line segment
asEventPts   :: LineSegment 2 p r :+ e -> [Event p r e]
asEventPts s = [ Event (s^.core.start.core) (Start $ s :| [])
               , Event (s^.core.end.core)   (End s)
               ]

-- | Group the segments with the intersection points
merge :: (Ord r, Fractional r) =>  [IntersectionPoint p r e] -> Intersections p r e
merge = foldr (\(IntersectionPoint p a) -> M.insertWith (<>) p a) M.empty

-- | Group the startpoints such that segments with the same start point
-- correspond to one event.
groupStarts                          :: Eq r => [Event p r e] -> [Event p r e]
groupStarts []                       = []
groupStarts (Event p (Start s) : es) = Event p (Start ss) : groupStarts rest
  where
    (ss',rest) = L.span sameStart es
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
data Event p r e = Event { eventPoint :: !(Point 2 r)
                         , eventType  :: !(EventType (LineSegment 2 p r :+ e))
                         } deriving (Show,Eq)

instance Ord r => Ord (Event p r e) where
  -- decreasing on the y-coord, then increasing on x-coord, and increasing on event-type
  (Event p s) `compare` (Event q t) = case ordPoints p q of
                                        EQ -> s `compare` t
                                        x  -> x

-- | Get the segments that start at the given event point
startSegs   :: Event p r e -> [LineSegment 2 p r :+ e]
startSegs e = case eventType e of
                Start ss -> NonEmpty.toList ss
                _        -> []

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * The Main Sweep

type EventQueue      p r e = EQ.Set (Event p r e)
type StatusStructure p r e = SS.Set (LineSegment 2 p r :+ e)

-- | Run the sweep handling all events
sweep       :: (Ord r, Fractional r)
            => EventQueue p r e -> StatusStructure p r e -> [IntersectionPoint p r e]
sweep eq ss = case EQ.minView eq of
    Nothing      -> []
    Just (e,eq') -> handle e eq' ss

-- | Handle an event point
handle                           :: forall r p e. (Ord r, Fractional r)
                                 => Event p r e -> EventQueue p r e -> StatusStructure p r e
                                 -> [IntersectionPoint p r e]
handle e@(eventPoint -> p) eq ss = toReport <> sweep eq' ss'
  where
    starts                   = startSegs e
    (before,contains',after) = extractContains p ss
    (ends,contains)          = L.partition (endsAt p) contains'
    -- starting segments, exluding those that have an open starting point
    -- starts' = filter (isClosedStart p) starts
    starts' = shouldReport p $ SS.toAscList newSegs

    -- If we just inserted open-ended segments that start here, then
    -- don't consider them to be "contained" segments.
    pureContains = filter (\(LineSegment s _ :+ _) ->
                              not $ isOpen s && p == s^.unEndPoint.core) contains

    -- any (closed) ending segments at this event point.
    closedEnds = filter (\(LineSegment _ e' :+ _) -> isClosed e') ends

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

-- | given the starting point p, and the segments that either start in
-- p, or continue in p, in left to right order along a line just
-- epsilon below p, figure out which segments we should report as
-- intersecting at p.
--
-- in partcular; those that:
-- - have a closed endpoint at p
-- - those that have an open endpoint at p and have an intersection
--   with a segment eps below p. Those segments thus overlap wtih
--   their predecessor or successor in the cyclic order.
shouldReport   :: (Ord r, Num r)
               => Point 2 r -> [LineSegment 2 p r :+ e] -> [LineSegment 2 p r :+ e]
shouldReport _ = overlapsOr (\(LineSegment s _ :+ _) -> isClosed s)
                            (\(s :+ _) (s2 :+ _) -> s `intersects` s2)

-- | split the status structure, extracting the segments that contain p.
-- the result is (before,contains,after)
extractContains      :: (Fractional r, Ord r)
                     => Point 2 r -> StatusStructure p r e
                     -> (StatusStructure p r e, [LineSegment 2 p r :+ e], StatusStructure p r e)
extractContains p ss = (before, F.toList mid1 <> F.toList mid2, after)
  where
    (before, mid1, after') = SS.splitOn (xCoordAt' $ p^.yCoord) (p^.xCoord) ss
    -- Make sure to also select the horizontal segments containing p
    (mid2, after) = SS.spanAntitone (intersects p . view core) after'
    xCoordAt' y sa = xCoordAt y (sa^.core)

-- | Given a point and the linesegements that contain it. Create a piece of
-- status structure for it.
toStatusStruct      :: (Fractional r, Ord r)
                    => Point 2 r -> [LineSegment 2 p r :+ e] -> StatusStructure p r e
toStatusStruct p xs = ss `SS.join` hors
  -- ss { SS.nav = ordAtNav $ p^.yCoord } `SS.join` hors
  where
    (hors',rest) = L.partition isHorizontal xs
    ss           = SS.fromListBy (ordAtY' $ maxY xs) rest
    hors         = SS.fromListBy (comparing rightEndpoint) hors'

    isHorizontal s  = s^.core.start.core.yCoord == s^.core.end.core.yCoord

    ordAtY' q sa sb = ordAtY q (sa^.core) (sb^.core)

    -- find the y coord of the first interesting thing below the sweep at y
    maxY = maximum . filter (< p^.yCoord)
         . concatMap (\s -> [s^.core.start.core.yCoord,s^.core.end.core.yCoord])

-- | Get the right endpoint of a segment
rightEndpoint   :: Ord r => LineSegment 2 p r :+ e -> r
rightEndpoint s = (s^.core.start.core.xCoord) `max` (s^.core.end.core.xCoord)

-- | Test if a segment ends at p
endsAt                                  :: Eq r => Point 2 r -> LineSegment 2 p r :+ e -> Bool
endsAt p (LineSegment' _ (b :+ _) :+ _) = p == b
  -- all (\q -> ordPoints (q^.core) p /= GT) [a,b]

--------------------------------------------------------------------------------
-- * Finding New events

-- | Find all events
findNewEvent       :: (Ord r, Fractional r)
                   => Point 2 r -> LineSegment 2 p r :+ e -> LineSegment 2 p r :+ e
                   -> Maybe (Event p r e)
findNewEvent p l r = match ((l^.core) `intersect` (r^.core)) $
     H (const Nothing) -- NoIntersection
  :& H (\q -> if ordPoints q p == GT then Just (Event q Intersection)
                                     else Nothing)
  :& H (const Nothing) -- full segment intersectsions are handled
                       -- at insertion time
  :& RNil



type R = Rational

seg1, seg2 :: LineSegment 2 () R
seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
seg2 = ClosedLineSegment (ext $ Point2 0 1) (ext $ Point2 0 5)



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
