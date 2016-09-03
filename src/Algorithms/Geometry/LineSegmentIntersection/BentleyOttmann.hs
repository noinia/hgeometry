module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann where

import Data.Ord(Down(..), comparing)
import Data.Function(on)
import Control.Lens hiding (contains)
import Data.Semigroup
import Data.Ext
import Data.Maybe
import Data.Geometry.Interval
import Data.Geometry.LineSegment hiding (Start,End)
import Data.Geometry.Point
import Data.Geometry.Line
import Data.Geometry.Properties
import Frames.CoRec
import Data.Vinyl
import Data.Proxy
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as L
import qualified Data.Set as EQ  -- event queue
import qualified Data.BalBST as SS  -- status struct

import Debug.Trace

--------------------------------------------------------------------------------

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

data Event p r = Event { eventPoint :: Point 2 r
                       , eventType  :: EventType (LineSegment 2 p r)
                       } deriving (Show,Eq)

instance Ord r => Ord (Event p r) where
  -- decreasing on the y-coord, then increasing on x-coord, and increasing on event-type
  (Event p s) `compare` (Event q t) = case ordPoints p q of
                                        EQ -> s `compare` t
                                        x  -> x

  -- (Event (Point2 a b) s) `compare` (Event (Point2 x y) t) = case b `compare` y of
  --                                                             GT -> LT
  --                                                             LT -> GT
  --                                                             EQ -> (a,s) `compare` (x,t)


-- | Decreasing on y, increasing on x
ordPoints     :: Ord r => Point 2 r -> Point 2 r -> Ordering
ordPoints a b = let f p = (Down $ p^.yCoord, p^.xCoord) in comparing f a b


startSegs   :: Event p r -> [LineSegment 2 p r]
startSegs e = case eventType e of
                Start ss -> NonEmpty.toList ss
                _        -> []


-- -- TODO: we have to group the p's for the start points
-- data GEvent q s = Start        !q !(NonEmpty.NonEmpty s)
--                 | Intersection !q  -- for intersections we find the actual intersecting
--                                    -- segments again anyway
--                 | End          !q !s
--                 deriving Show

-- constr (Start _ _)      = 1
-- constr (Intersection _) = 2
-- constr (End _ _)        = 3

-- instance Eq q => Eq (GEvent q s) where
--   e == e' = (eventPoint e, constr e) == (eventPoint e', constr e')


--   -- (Start p _)      == (Start q _)      = p == q
--   -- (Intersection p) == (Intersection q) = p == q
--   -- (End p _)        == (End q _)        = p == q
--   -- _                == _                = False
--   -- -- e == e' = eventPoint e == eventPoint e'

-- instance Ord q => Ord (GEvent q s) where
--   e `compare` e' = (eventPoint e, constr e) `compare` (eventPoint e', constr e')




--     -- if e == e' then EQ else  eventPoint e `compare` eventPoint e'


-- type Event p r = GEvent (Point 2 r) (LineSegment 2 p r)

-- | Order at a horizontal line
ordAtNav   :: (Ord r, Fractional r) => r -> SS.TreeNavigator r (LineSegment 2 p r)
ordAtNav y = SS.Nav (\s x -> h s <= x) (min `on` h)
  where
    h s = match (s `intersect` horizontalLine y) $
         (H $ \NoIntersection -> error "ordAtNav: No intersection")
      :& (H $ \p              -> p^.xCoord)
      :& (H $ \s              -> rightEndpoint s)
      :& RNil

    --   let Just p = asA (Proxy :: Proxy (Point 2 r)) $
    --       in p^.xCoord
    -- -- FIXME: this fails for horizontal segments

-- eventPoint             :: GEvent q s -> q
-- eventPoint (Start p _) = p
-- eventPoint (Intersection p) = p
-- eventPoint (End p _) = p


-- startSegs              :: Event p r -> [LineSegment 2 p r]
-- startSegs (Start _ ss) = NonEmpty.toList ss
-- startSegs _            = []

-- isEnd (End _ _) = True
-- isEnd _         = False



data IntersectionPoint p r =
  IntersectionPoint { _intersectionPoint :: Point 2 r
                    , _endPointOf        :: [LineSegment 2 p r]
                    , _interiorTo        :: [LineSegment 2 p r]
                    } deriving (Show,Eq)



intersections    :: (Ord r, Fractional r, Show r, Show p)
                 => [LineSegment 2 p r] -> [IntersectionPoint p r]
intersections ss = sweep pts (SS.empty $ ordAtNav undefined)
  where
    pts = EQ.fromList . groupStarts . concatMap f $ ss
    f s = let [p,q] = L.sortBy ordPoints [s^.start.core,s^.end.core]
          in [Event p (Start $ s :| []), Event q (End s)]

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


type EventQueue p r = EQ.Set (Event p r)
type StatusStructure p r = SS.BalBST r (LineSegment 2 p r)

sweep       :: (Ord r, Fractional r, Show r, Show p)
            => EventQueue p r -> StatusStructure p r -> [IntersectionPoint p r]
sweep eq ss = case EQ.minView eq of
    Nothing      -> []
    Just (e,eq') -> handle e eq' ss

l = SS.toList


handle                           :: (Ord r, Fractional r, Show r, Show p)
                                 => Event p r -> EventQueue p r -> StatusStructure p r
                                 -> [IntersectionPoint p r]
handle e@(eventPoint -> p) eq ss
  | traceShow ("=====================\nHandling, ", e, SS.toList $ ss) False = undefined
handle e@(eventPoint -> p) eq ss = toReport <> sweep eq' ss'
  where
    starts                   = traceShowId $ startSegs e
    ttt = extractContains p ss
    (before,contains',after) = traceShow ("PARTS::::: ",ttt) ttt
    (ends,contains)          = L.partition (endsAt p) contains'


    toReport = case starts ++ contains' of
                 (_:_:_) -> [IntersectionPoint p (starts <> ends) contains]
                 _       -> []

    -- new status structure
    ss' = traceShow ("new status: ", l ss'', "====", l before ++ l newSegs ++ l after) ss''
    ss'' = before `SS.join` newSegs `SS.join` after

    newSegs = traceShow ("new segs: ", SS.toList $ newSegs') newSegs'
    newSegs' = toStatusStruct p $ starts ++ contains

    -- the new eeventqueue
    eq' = foldr EQ.insert eq es'
    es'  = traceShow ("New events found: ", es) es

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


rightEndpoint s = (s^.start.core.xCoord) `max` (s^.end.core.xCoord)




-- | Test if a segment ends at p
endsAt                      :: Ord r => Point 2 r -> LineSegment 2 p r -> Bool
endsAt p (LineSegment' a b) = all (\q -> ordPoints (q^.core) p /= GT) [a,b]

-- | Find all events
findNewEvent       :: (Ord r, Fractional r, Show r, Show p)
                   => Point 2 r -> LineSegment 2 p r -> LineSegment 2 p r
                   -> Maybe (Event p r)

findNewEvent p l r
  | traceShow ("FindNewEvent: ", p, l, r) False = undefined
findNewEvent p l r = match (l `intersect` r) $
     (H $ \NoIntersection -> Nothing)
  :& (H $ \q              -> if ordPoints q p == GT then Just (Event q Intersection)
                                      else Nothing)
  :& (H $ \_              -> Nothing) -- full segment intersectsions are handled
                                      -- at insertion time
  :& RNil

-- | split the status structure, extracting the segments that contain p.
-- the result is (before,contains,after)
extractContains      :: (Fractional r, Ord r, Show r, Show p)
                     => Point 2 r -> StatusStructure p r
                     -> (StatusStructure p r, [LineSegment 2 p r], StatusStructure p r)
extractContains p ss = (before, mid1 ++ mid2, after)
  where
    n = ordAtNav (p^.yCoord)
    SS.Split before (mid1,mid2) after = SS.splitExtract pred' sel $ ss { SS.nav = n}

    pred' s = not $ SS.goLeft n s (p^.xCoord)
    sel   s = p `onSegment` s

testSs :: StatusStructure () Rational
testSs = SS.fromList (ordAtNav 5) [ ClosedLineSegment (point2 10 0 :+ ()) (point2 10 10 :+ ()) ]

test = extractContains (point2 5 5) testSs
