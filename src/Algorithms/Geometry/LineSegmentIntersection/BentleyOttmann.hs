module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann where

import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens hiding (contains)
import qualified Data.BalBST as SS -- status struct
import           Data.Ext
import           Data.Function (on)
import           Data.Geometry.Interval
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord (Down(..), comparing)
import           Data.Semigroup
import qualified Data.Set as EQ -- event queue
import           Data.Vinyl
import           Frames.CoRec

import           Debug.Trace

--------------------------------------------------------------------------------

-- | Compute all intersections
--
-- $O((n+k)\log n)$, where $k$ is the number of intersections.
intersections    :: (Ord r, Fractional r)
                 => [LineSegment 2 p r] -> Intersections p r
intersections ss = merge $ sweep pts (SS.empty $ ordAtNav undefined)
  where
    pts = EQ.fromAscList . groupStarts . L.sort . concatMap f $ ss
    f s = let [p,q] = L.sortBy ordPoints [s^.start.core,s^.end.core]
          in [Event p (Start $ s :| []), Event q (End s)]

-- | Group the segments with the intersection points
merge :: Ord r =>  [IntersectionPoint p r] -> Intersections p r
merge = foldr (\(IntersectionPoint p a) -> M.insertWith (<>) p a) M.empty

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
groupStarts (e : es)           = e : groupStarts es


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

-- | The navigator that we use that orders the segments that intersect at a
-- horizontal line (from left to right)
ordAtNav   :: (Ord r, Fractional r) => r -> SS.TreeNavigator r (LineSegment 2 p r)
ordAtNav y = SS.Nav (\s x -> h s <= x) (min `on` h)
  where
    h s = match (s `intersect` horizontalLine y) $
         (H $ \NoIntersection -> error "ordAtNav: No intersection")
      :& (H $ \p              -> p^.xCoord)
      :& (H $ \_              -> rightEndpoint s) -- the intersection is s itself
      :& RNil


--------------------------------------------------------------------------------
-- * The Main Sweep

type EventQueue p r = EQ.Set (Event p r)

type StatusStructure p r = SS.BalBST r (LineSegment 2 p r)

-- | Run the sweep handling all events
sweep       :: (Ord r, Fractional r)
            => EventQueue p r -> StatusStructure p r -> [IntersectionPoint p r]
sweep eq ss = case EQ.minView eq of
    Nothing      -> []
    Just (e,eq') -> handle e eq' ss

-- | Handle an event point
handle                           :: (Ord r, Fractional r)
                                 => Event p r -> EventQueue p r -> StatusStructure p r
                                 -> [IntersectionPoint p r]
handle e@(eventPoint -> p) eq ss = toReport <> sweep eq' ss'
  where
    starts                   = startSegs e
    (before,contains',after) = extractContains p ss
    (ends,contains)          = L.partition (endsAt p) contains'


    toReport = case starts ++ contains' of
                 (_:_:_) -> [IntersectionPoint p $ associated (starts <> ends) contains]
                 _       -> []

    -- new status structure
    ss' = before `SS.join` newSegs `SS.join` after

    newSegs = toStatusStruct p $ starts ++ contains

    -- the new eeventqueue
    eq' = foldr EQ.insert eq es

    -- the new events:
    es | SS.null newSegs = maybeToList $ app (findNewEvent p) sl sr
       | otherwise       = let s'  = fst <$> SS.minView newSegs
                               s'' = fst <$> SS.maxView newSegs
                           in catMaybes [ app (findNewEvent p) sl  s'
                                        , app (findNewEvent p) s'' sr
                                        ]
    sl = fst <$> SS.maxView before
    sr = fst <$> SS.minView after

    app f x y = do { x' <- x ; y' <- y ; f x' y'}

-- | split the status structure, extracting the segments that contain p.
-- the result is (before,contains,after)
extractContains      :: (Fractional r, Ord r)
                     => Point 2 r -> StatusStructure p r
                     -> (StatusStructure p r, [LineSegment 2 p r], StatusStructure p r)
extractContains p ss = (before, mid1 ++ mid2, after)
  where
    n = ordAtNav (p^.yCoord)
    SS.Split before (mid1,mid2) after = SS.splitExtract pred' sel $ ss { SS.nav = n}

    pred' s = not $ SS.goLeft n s (p^.xCoord)
    sel   s = p `onSegment` s


-- | Given a point and the linesegements that contain it. Create a piece of
-- status structure for it.
toStatusStruct      :: (Fractional r, Ord r)
                    => Point 2 r -> [LineSegment 2 p r] -> StatusStructure p r
toStatusStruct p xs = ss { SS.nav = ordAtNav $ p^.yCoord } `SS.join` hors
  where
    (hors',rest) = L.partition isHorizontal xs
    ss           = SS.fromList (ordAtNav $ maxY xs) rest
    hors         = SS.fromList (SS.ordNavBy rightEndpoint) hors'

    isHorizontal s  = s^.start.core.yCoord == s^.end.core.yCoord

    -- find the y coord of the first interesting thing below the sweep at y
    maxY = maximum . filter (< p^.yCoord)
         . concatMap (\s -> [s^.start.core.yCoord,s^.end.core.yCoord])

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
     (H $ \NoIntersection -> Nothing)
  :& (H $ \q              -> if ordPoints q p == GT then Just (Event q Intersection)
                                      else Nothing)
  :& (H $ \_              -> Nothing) -- full segment intersectsions are handled
                                      -- at insertion time
  :& RNil
