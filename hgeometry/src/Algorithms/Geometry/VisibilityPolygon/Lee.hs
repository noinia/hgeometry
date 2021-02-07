{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.VisibilityPolygon.Lee
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(O(n\log n)\) time algorithm to compute the visibility polygon of
-- a point inside a polygon (possibly containing holes) with \(n\)
-- vertices, or among a set of \(n\) disjoint segments. The alogirhtm
-- used is the the rotational sweepline algorithm by Lee, described
-- in:
--
-- D. T. Lee. Proximity and reachability in the plane. Report R-831, Dept. Elect.
-- Engrg., Univ. Illinois, Urbana, IL, 1978.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.VisibilityPolygon.Lee where

import           Control.Lens
import           Control.Monad ((<=<))
import           Data.Bifunctor (first, second)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.HalfLine
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Vector
import           Data.Intersection
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe, isJust)
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable
import qualified Data.Set as Set
import qualified Data.Set.Util as Set
import           Data.Vinyl.CoRec
import           Debug.Trace
import           Data.RealNumber.Rational

type R = RealNumber 5

--------------------------------------------------------------------------------

type StarShapedPolygon p r = SimplePolygon p r

data Action a = Insert a | Delete a deriving (Show,Eq,Ord)

isInsert :: Action a -> Bool
isInsert = \case
  Insert _ -> True
  Delete _ -> False

extract :: Action a -> a
extract = \case
  Insert x -> x
  Delete x -> x


-- | An event corresponds to some orientation at which the set of segments
-- intersected by the ray changes (this orientation is defined by a point)
data Event p e r = Event { _eventVtx :: Point 2 r :+ p
                         , _actions  :: NonEmpty (Action (LineSegment 2 p r :+ e))
                         } deriving Show
makeLenses ''Event

-- | The status structure maintains the subset of segments currently
-- intersected by the ray that starts in the query point q, in order
-- of increasing distance along the ray.
type Status p e r = Set.Set (LineSegment 2 p r :+ e)


-- | Vertices of the visibility polgyon are either original vertices
-- or defined by some vertex and an edge
type Definer p e r = Either p (Point 2 r :+ p,LineSegment 2 p r :+ e)

--------------------------------------------------------------------------------

-- | Computes the visibility polygon of a point q in a polygon with
-- \(n\) vertices.
--
-- running time: \(O(n\log n)\)
visibilityPolygon   :: forall p t r. (Ord r, Fractional r, Show r, Show p)
                    => Point 2 r
                    -> Polygon t p r
                    -> StarShapedPolygon (Definer p (p,p) r) r
visibilityPolygon q = fromPoints . visibilityPolygonFromTo Nothing q . closedEdges

closedEdges :: Polygon t p r -> [LineSegment 2 p r :+ (p,p)]
closedEdges = map asClosed . listEdges
  where
    asClosed (LineSegment' u v) = ClosedLineSegment u v :+ (u^.extra,v^.extra)

-- | computes the visibility polygon of a set of \(n\) disjoint
-- segments. The input segments are allowed to share endpoints, but no
-- intersections or no endpoints in the interior of other segments.
--
-- pre : - all line segments are considered closed.
--       - no singleton linesegments exactly pointing away from q.
--       - for every orientattion the visibility is blocked somewhere, i.e.
--            no rays starting in the query point q that are disjoint from all segments.
--
-- running time: \(O(n\log n)\)
visibilityPolygonFromTo            :: forall p r e. (Ord r, Fractional r, Show r, Show p, Show e)
                                   => Maybe (Point 2 r, Point 2 r)
                                   -- ^ pair of points indicating the CCW range to
                                   -- compute the visibility in. If Nothing use the whole range
                                   -> Point 2 r -- ^ the point form which we compute the visibility polgyon
                                   -> [LineSegment 2 p r :+ e]
                                   -> [Point 2 r :+ Definer p e r]
visibilityPolygonFromTo rng q segs = snd $ sweep (traceShowId $ events)
  where
    -- lazily test if the segment intersects the initial ray
    segs' = map (\(s :+ e) -> s :+ (initialIntersection q initialRay s, e)) segs

    events@(e0:e1:_) = map (combine q)
                     . groupBy' (\a b -> ccwCmpAroundWith' sv (ext q) (a^.eventVtx) (b^.eventVtx))
                     . untilEnd
                     . concatMap (mkEvent sv q) $ segs'

        -- take only until the end of the range (if defined)
    untilEnd = maybe id
                     (\(_,t) -> List.takeWhile (\e ->
                                  ccwCmpAroundWith' sv (ext q) (e^.eventVtx) (ext t) == LT))
                     rng
    sv = maybe (Vector2 1 0) (\(s,_) -> s .-. q) rng

    initialRay = HalfLine q (startingDirection q (e0^.eventVtx.core) (e1^.eventVtx.core))

    statusStruct = traceShowId $ fromListByDistTo q segs'

    sweep = List.foldl' (handleEvent q) (statusStruct,[])

-- | Given multiple events happening at the same orientation, combine
-- them into a single event.
combine      :: (Ord r, Num r) => Point 2 r -> NonEmpty (Event p e r) -> Event p e r
combine q es = Event p acts
  where
    acts = foldMap1 (^.actions) es
    p    = F.minimumBy (cmpByDistanceTo' (ext q)) . fmap (^.eventVtx) $ es


-- | Computes the direction of the initial ray
startingDirection       :: Fractional r => Point 2 r -> Point 2 r -> Point 2 r -> Vector 2 r
startingDirection q u w = v .-. q
  where
    v = u .+^ ((w .-. u) ^/ 2) -- point in the middle between u and w
        -- note: the segment between u and w could pass on the wrong side of q
        -- (i.e. so that does not "cover" the CCW but the CW range between u and w)
        -- however, in that case there is apparently nothing on the CCW side opposite
        -- to v, as u and w are supposed to be the first two events. This means the
        -- precondition does not hold.

allEndPoints      :: (Ord r, Num r)
                  => Vector 2 r -> Point 2 r -> [LineSegment 2 p r :+ e]
                  -> [Point 2 r :+ (LineSegment 2 p r :+ e)]
allEndPoints sv q = List.sortBy cmp
                  . concatMap (\s@((LineSegment' u v) :+ _) -> [(u^.core) :+ s, (v^.core) :+ s])
  where
    cmp = ccwCmpAroundWith' sv (ext q) <> cmpByDistanceTo' (ext q)

-- -- | Computes the events in the sweep. The events are recorded in CCW
-- -- order, starting from the horizontal line to the right.
-- endPoints           :: (Ord r, Fractional r, Show r, Show p, Show e)
--                     => Maybe (Point 2 r, Point 2 r) -- ^ from, to
--                     -> Point 2 r
--                     -> [LineSegment 2 p r :+ e]
--                     -> [ Point 2 r :+


--   NonEmpty
--                        ]
-- endPoints q segs = groupBy'   (ccwCmpAroundWith' sv (ext q))
--                  . allEndPoints
--   where
--     pts = untilEnd $ allEndPoints segs




mkEvent                                      :: (Ord r, Num r)
                                             => Vector 2 r
                                             -> Point 2 r
                                             -> LineSegment 2 p r :+ (Maybe r, e)
                                             -> [Event p e r]
mkEvent sv q (s@(LineSegment' u v) :+ (d,e)) = case cmp u v of
                                            LT -> [ Event u insert
                                                  , Event v delete
                                                  ]
                                            GT -> [ Event v insert
                                                  , Event u delete
                                                  ]
                                            EQ -> [] -- zero length segment, just skip
  where
    cmp = ccwCmpAroundWith' sv (ext q) <> cmpByDistanceTo' (ext q)
    s'  = s :+ e

    insert = (if isJust d then Delete s' else Insert s') :| []
    delete = (if isJust d then Insert s' else Delete s') :| []





-- -- | Gets the combinatorial representation of the visibility polygon
-- toCombinatorial    :: StarShapedPolygon (Definer p e r) r -> CVec.CircularVector (Either p (p,e))
-- toCombinatorial pg = fmap (second f . (^.extra)) $ pg^.outerBoundaryVector
--   where
--     f = bimap (^.extra) (^.extra)

----------------------------------------

-- -- | Test if we should insert or delete a segment
-- determineEventType                  :: (Ord r, Fractional r, Show r, Show p, Show e)
--                                     => Point 2 r
--                                     -> (Point 2 r :+ (p, Point 2 r :+ p, LineSegment 2 p r :+ e))
--                                     -> Maybe (Action (LineSegment 2 p r :+ e))
-- determineEventType q (u :+ (_,v,s)) =
--     case (ccwCmpAround' (ext q) (ext u) v, isJust $ initialIntersection q (q .+^ Vector2 1 0) s) of
--       (EQ, _)     -> Nothing -- colinear, so do nothing
--       (LT, False) -> Just $ Insert s -- normal mode && u before v => insertion at u
--       (GT, False) -> Just $ Delete s  -- normal mode && u after v  => deletion  at u
--       (LT, True)  -> if v^.core.yCoord > q^.yCoord then Just $ Insert s
--                                                    else Just $ Delete s
--                      -- u appears before v, and we intersect the initial ray,
--                      -- that must either mean that:
--                      --  * u lies *on* the ray, v lies strictly above the ray
--                      --          => insertion at u
--                      --  * u lies above (or on) the ray, v lies strictly below the ray
--                      --          => deletion at u

--       (GT, True)  -> if u^.yCoord > q^.yCoord then Just $ Deletion s
--                                               else Just $ Insertion s




-- -- | Computes the right events happening at this slope
-- mkEvent               :: (Ord r, Fractional r, Show r, Show p, Show e)
--                       => Point 2 r
--                       -> NonEmpty (Point 2 r :+ (p, Point 2 r :+ p, LineSegment 2 p r :+ e))
--                       -> Event p e r
-- mkEvent q ps@(p :| _) = Event (p&extra %~ \(e,_,_) -> e) ins dels
--   where
--     (ins,dels) = foldr (\p' acc@(ins',dels') -> case determineEventType q p' of
--                                                   Nothing          -> acc
--                                                   Just (Insert s)  -> (s:ins',dels')
--                                                   Just (Delete  s) -> (ins',s:dels')
--                        ) ([],[]) ps
--     -- FIXME: maybe we shoulnd't just return the first p in ps, but
--     -- all of them.  if we want to do that we should make sure the
--     -- points are orderd by increasing distance from p when sorting the points
--     --
--     -- FIXME: if q is a vertex in ps our partitioning stuff probably goes wrong






-- | Handles an event, computes the new status structure and output polygon.
handleEvent                                  :: (Ord r, Fractional r, Show r, Show p, Show e)
                                             => Point 2 r
                                             -> (Status p e r, [Point 2 r :+ Definer p e r])
                                             -> Event p e r
                                             -> (Status p e r, [Point 2 r :+ Definer p e r])
handleEvent q (ss,out) (Event (p :+ z) acts) = (ss', newVtx <> out)
  where
    (ins,dels) = bimap (map extract) (map extract) . NonEmpty.partition isInsert $ acts

    ss' = flip (foldr (insertAt q p)) ins
        . flip (foldr (deleteAt q p)) dels
        $ ss

    newVtx = let (a :+ sa) = firstHitAt' q p ss
                 (b :+ sb) = firstHitAt' q p ss'
                 ae        = valOf a sa
                 be        = valOf b sb
             in case (a /= b, a == p) of
                  (True, _)     -> -- new window of the output polygon discovered
                                   -- figure out who is the closest vertex, (the reflex vtx)
                                   -- and add the appropriate two vertices
                    case squaredEuclideanDist q a < squaredEuclideanDist q b of
                      True  -> [ b :+ Right (a :+ ae, sb)
                               , a :+ Left  ae  -- a must be a vertex!
                               ]
                      False -> [ b :+ Left  be
                               , a :+ Right (b :+ be, sa)
                               ]
                  (False,True)  -> [ p :+ Left z]
                    -- sweeping over a regular vertex of the visibility polygon
                  (False,False) -> []    -- sweeping over a vertex not in output

    valOf a (LineSegment' (b :+ be) (_ :+ ce) :+ _ ) | a == b    = be
                                                     | otherwise = ce



--------------------------------------------------------------------------------

-- | Given two points q and p, and a status structure retrieve the
-- first segment in the status structure intersected by the ray from q
-- through p.
--
-- pre: all segments in the status structure should intersect the ray
--      from q through p (in a point), in that order.
--
-- running time: \(O(\log n)\)
firstHitAt     :: forall p r e. (Ord r, Fractional r, Show r, Show p, Show e)
               => Point 2 r -> Point 2 r
               -> Status p e r
               -> Maybe (Point 2 r :+ LineSegment 2 p r :+ e)
firstHitAt q p = computeIntersectionPoint <=< Set.lookupMin
  where
    computeIntersectionPoint s = fmap (:+ s) . asA @(Point 2 r)
                               $ supportingLine (s^.core) `intersect` lineThrough p q

-- | Given two points q and p, and a status structure retrieve the
-- first segment in the status structure intersected by the ray from q
-- through p.
--
-- pre: - all segments in the status structure should intersect the ray
--        from q through p (in a point), in that order.
--      - the status structure is non-empty
--
-- running time: \(O(\log n)\)
firstHitAt'        :: forall p r e. (Ord r, Fractional r, Show r, Show p, Show e)
                  => Point 2 r -> Point 2 r
                  -> Status p e r
                  -> Point 2 r :+ LineSegment 2 p r :+ e
firstHitAt' q p s = case firstHitAt q p s of
                      Just x  -> x
                      Nothing -> error $ "firstHitAt: precondition failed!" <> show (p,q,s)

--------------------------------------------------------------------------------
-- * Status Structure Operations

-- | Insert a new segment into the status structure, depending on the
-- (distance from q to to the) intersection point with the ray from q
-- through p
--
-- pre: all segments in the status structure should intersect the ray
--      from q through p, in that order.
--
-- \(O(\log n)\)
insertAt     :: (Ord r, Fractional r, Show r, Show p)
             => Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
insertAt q p = Set.insertBy (compareByDistanceToAt q p <> flip compareAroundEndPoint)
  -- if two segments have the same distance, they must share and endpoint
  -- so we use the CCW ordering around this common endpoint to determine
  -- the order.

-- | Delete a segment from the status structure, depending on the
-- (distance from q to to the) intersection point with the ray from q
-- through p
--
-- pre: all segments in the status structure should intersect the ray
--      from q through p, in that order.
--
-- \(O(\log n)\)
deleteAt     :: (Ord r, Fractional r, Show r, Show p, Show e)
             => Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
deleteAt q p = Set.deleteAllBy (compareByDistanceToAt q p <> compareAroundEndPoint)
  -- if two segments have the same distance, we use the ccw order around their common
  -- (end) point.

-- FIXME: If there are somehow segmetns that would continue at p as
-- well, they are also deleted.


-- | Given a point q compute the subset of segments intersecting the
-- horizontal rightward ray starting in q, and order them by
-- increasing dsitance.
fromListByDistTo   :: forall r p e. (Ord r, Fractional r)
                   => Point 2 r -> [ LineSegment 2 p r :+ (Maybe r, e)]
                   -> Status p e r
fromListByDistTo q = Set.mapMonotonic (^.extra)
                   . foldr (Set.insertBy $ comparing (^.core)) Set.empty
                   . mapMaybe (\(s :+ (md,e)) -> (:+ (s :+ e)) <$> md)

-- | Given q and a segment s, computes if the segment intersects the initial, rightward
-- ray starting in q, and if so returns the (squared) distance from q to that point
-- together with the segment.
initialIntersection         :: forall r p e. (Ord r, Fractional r)
                            => Point 2 r -> HalfLine 2 r -> LineSegment 2 p r
                            -> Maybe r
initialIntersection q ray s =
    case asA @(Point 2 r) $ seg `intersect` ray of
      Nothing -> Nothing
      Just z  -> Just $ squaredEuclideanDist q z
  where
    seg = first (const ()) s

--------------------------------------------------------------------------------
-- * Comparators for the rotating ray

-- | Given two points q and p, and two segments a and b that are guaranteed to
-- intersect the ray from q through p once, order the segments by their
-- intersection point
compareByDistanceToAt     :: forall p r e. (Ord r, Fractional r)
                          => Point 2 r -> Point 2 r
                          -> LineSegment 2 p r :+ e
                          -> LineSegment 2 p r :+ e
                          -> Ordering
compareByDistanceToAt q p = comparing f
  where
    f (s :+ _) = fmap (squaredEuclideanDist q)
               . asA @(Point 2 r)
               $ supportingLine s `intersect` lineThrough p q

-- | Given two segments that share an endpoint, order them by their
-- order around this common endpoint. I.e. if uv and uw share endpoint
-- u we uv is considered smaller iff v is smaller than w in the
-- counterclockwise order around u (treating horizontal rightward as
-- zero).
compareAroundEndPoint  :: forall p r e. (Ord r, Fractional r, Show r, Show p)
                       => LineSegment 2 p r :+ e
                       -> LineSegment 2 p r :+ e
                       -> Ordering
compareAroundEndPoint (sa@(LineSegment' p q) :+ _)
                      (sb@(LineSegment' s t) :+ _)
    -- traceshow ("comapreAroundEndPoint ", sa, sb) False = undefined
    | p^.core == s^.core = ccwCmpAround' p q t
    | p^.core == t^.core = ccwCmpAround' p q s
    | q^.core == s^.core = ccwCmpAround' q p t
    | q^.core == t^.core = ccwCmpAround' q p s
    | otherwise          = error $ "compareAroundEndPoint: precondition failed!" <> show (sa,sb)


-- -- | Get the other endpoint of the segment
-- otherEndPoint     :: Eq r => Point 2 r -> LineSegment 2 p r -> Maybe (Point 2 r :+ p)
-- otherEndPoint p (LineSegment' a b) | p == a^.core = Just b
--                                    | p == b^.core = Just a
--                                    | otherwise    = Nothing

--------------------------------------------------------------------------------
-- * Generic Helper functions

-- | Given a function f, partitions the list into three lists
-- (lts,eqs,gts) such that:
--
-- - f x == LT for all x in lts
-- - f x == EQ for all x in eqs
-- - f x == gt for all x in gts
--
-- >>> partition3 (compare 4) [0,1,2,2,3,4,5,5,6,6,7,7,7,7,7,8]
-- ([5,5,6,6,7,7,7,7,7,8],[4],[0,1,2,2,3])
--
partition3   :: Foldable f => (a -> Ordering) -> f a -> ([a],[a],[a])
partition3 f = foldr g ([],[],[])
  where
    g x (lts,eqs,gts) = case f x of
                          LT -> (x:lts,   eqs,  gts)
                          EQ -> (  lts, x:eqs,  gts)
                          GT -> (  lts,   eqs,x:gts)

-- | A version of groupBy that uses the given Ordering to group
-- consecutive Equal items
--
-- >>> groupBy' compare [0,1,2,2,3,4,5,5,6,6,7,7,7,7,7,8]
-- [0 :| [],1 :| [],2 :| [2],3 :| [],4 :| [],5 :| [5],6 :| [6],7 :| [7,7,7,7],8 :| []]
groupBy'     :: (a -> a -> Ordering) -> [a] -> [NonEmpty a]
groupBy' cmp = go
  where
    go = \case
      []       -> []
      (x:xs)   -> let (pref,rest) = List.span (\y -> x `cmp` y == EQ) xs
                  in (x :| pref) : go rest


--------------------------------------------------------------------------------

test :: StarShapedPolygon (Definer Int (Int,Int) R) R
test = visibilityPolygon origin testPg

testPg :: SimplePolygon Int R
testPg = fromPoints $ zipWith (:+) [ Point2 3    1
                                   , Point2 3    2
                                   , Point2 4    2
                                   , Point2 2    4
                                   , Point2 (-1) 4
                                   , Point2 1    2
                                   , Point2 (-3) (-1)
                                   , Point2 4    (-1)
                                   ] [1..]

testPg2 :: SimplePolygon Int R
testPg2 = fromPoints $ zipWith (:+) [ Point2 3    1
                                    , Point2 3    2
                                    , Point2 4    2
                                    , Point2 2    4
                                    , Point2 (-1) 4
                                    , Point2 1    2.1
                                    , Point2 (-3) (-1)
                                    , Point2 4    (-1)
                                    ] [1..]



-- SimplePolygon (CSeq

-- [Point2 [4,-1] :+ Left 8
-- ,Point2 [-3,-1] :+ Left 7
-- ,Point2 [2,4] :+ Left 4
-- ,Point2 [1,2] :+ Right (Point2 [2,4] :+ 4,LineSegment (Closed (Point2 [1,2] :+ 6)) (Closed (Point2 [-3,-1] :+ 7)) :+ ())
-- ,Point2 [3,2] :+ Left 2
-- ,Point2 [3.6,2.4] :+ Right (Point2 [3,2] :+ 2,LineSegment (Closed (Point2 [4,2] :+ 3)) (Closed (Point2 [2,4] :+ 4)) :+ ())
-- ,Point2 [3,1] :+ Left 1])

testz :: SimplePolygon () R
testz = fromPoints $ read "[Point2 [144,640] :+ (),Point2 [64,640] :+ (),Point2 [128,592] :+ (),Point2 [224,656] :+ (),Point2 [256,592] :+ (),Point2 [320,656] :+ (),Point2 [272,752] :+ (),Point2 [208,688] :+ (),Point2 [176,768] :+ (),Point2 [112,688] :+ ()]"

queryP :: Point 2 R
queryP = Point2 136 616

query2 :: Point 2 R
query2 = Point2 252 704

spike :: SimplePolygon () R
spike = traceShowId $ read "SimplePolygon [Point2 160 656 :+ (),Point2 288 640 :+ (),Point2 320 704 :+ (),Point2 368 640 :+ (),Point2 368 736 :+ (),Point2 288 752 :+ (),Point2 256 704 :+ (),Point2 224 768 :+ ()]"
