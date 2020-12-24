
-- D. T. Lee. Proximity and reachability in the plane. Report R-831, Dept. Elect.
-- Engrg., Univ. Illinois, Urbana, IL, 1978.

module Algorithms.Geometry.VisibilityPolygon.Lee where

import           Control.Lens
import           Control.Monad ((<=<))
import           Data.Bifunctor (second)
import qualified Data.CircularSeq as CSeq
import           Data.Ext
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Intersection
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (mapMaybe, isJust)
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Set.Util as Set
import           Data.Vinyl.CoRec

-- import           Debug.Trace
import           Data.RealNumber.Rational

type R = RealNumber 5

--------------------------------------------------------------------------------

type StarShapedPolygon p r = SimplePolygon p r


data EventKind = Insert | Delete deriving (Show,Eq,Ord)

-- | An event corresponds to some orientation at which the set of segments
-- intersected by the ray changes (this orientation is defined by a point)
data Event p e r = Event { _eventVtx :: Point 2 r :+ p
                         , _toInsert :: [LineSegment 2 p r :+ e]
                         , _toDelete :: [LineSegment 2 p r :+ e]
                         } deriving Show

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
visibilityPolygon q = visibilityPolygon' q . map asClosed . listEdges
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
visibilityPolygon'        :: forall p r e. (Ord r, Fractional r, Show r, Show p, Show e)
                          => Point 2 r
                          -> [LineSegment 2 p r :+ e]
                          -> StarShapedPolygon (Definer p e r) r
visibilityPolygon' q segs = fromPoints . reverse . snd
                          $ List.foldl' (handleEvent q) (statusStruct,[]) events
  where
    statusStruct = fromListByDistTo q segs

    events    :: [Event p e r]
    events    = map (mkEvent q)
              . groupBy'    (ccwCmpAround (ext q))
              . List.sortBy (ccwCmpAround (ext q) <> cmpByDistanceTo (ext q))
              $ endPoints'

    endPoints' = concatMap (\s@(LineSegment' u v :+ _) -> [ u&extra %~ (,v,s)
                                                          , v&extra %~ (,u,s)
                                                          ]
                           ) segs


-- | Gets the combinatorial representation of the visibility polygon
toCombinatorial    :: StarShapedPolygon (Definer p e r) r -> CSeq.CSeq (Either p (p,e))
toCombinatorial pg = fmap (second f . (^.extra)) $ pg^.outerBoundary
  where
    f = bimap (^.extra) (^.extra)


----------------------------------------

-- | Computes the right events happening at this slope
mkEvent               :: (Ord r, Fractional r)
                      => Point 2 r
                      -> NonEmpty (Point 2 r :+ (p, Point 2 r :+ p, LineSegment 2 p r :+ e))
                      -> Event p e r
mkEvent q ps@(p :| _) = Event (p&extra %~ \(e,_,_) -> e) (ins <> extraIns) (dels <> extraDels)
  where
    -- figure out which segments start at u or end at u; segments
    -- for which u appears "before" the other endpoint v sould
    -- typically be inserted, whereas segments where u appears after v
    -- should typically be deleted.
    (ins',_colinears,dels') = partition3 (\(u :+ (_,v,_)) -> ccwCmpAround (ext q) (ext u) v) ps

    -- if a segment s=uv would intersect the initial, rightward ray, it is currently classified
    -- as "insert at u", since u would appear before v in the ordering. However, as s
    -- is already in the initial status structure, we should categorize s as a deletion instead.
    (extraDels,ins) = handleInitial ins'
    (extraIns,dels) = handleInitial dels'

    handleInitial = List.partition (isJust . initialIntersection q)
                  . map (\(_ :+ (_,_,s)) -> s)

    -- FIXME: maybe we shoulnd't just return the first p in ps, but
    -- all of them.  if we want to do that we should make sure the
    -- points are orderd by increasing distance from p when sorting the points
    --

    -- FIXME: if q is a vertex in ps our partitioning stuff probably goes wrong


-- | Handles an event, computes the new status structure and output polygon.
handleEvent                                     :: (Ord r, Fractional r, Show r, Show p, Show e)
                                                => Point 2 r
                                                -> (Status p e r, [Point 2 r :+ Definer p e r])
                                                -> Event p e r
                                                -> (Status p e r, [Point 2 r :+ Definer p e r])
-- handleEvent q (ss,out) e | traceShow ("handle", e, ss, out) False = undefined
handleEvent q (ss,out) (Event (p :+ z) is dels) = (ss', newVtx <> out)
  where
    ss' = flip (foldr (insertAt q p)) is
        . flip (foldr (deleteAt q p)) dels
        $ ss

    newVtx = let (a :+ sa) = firstHitAt' q p ss
                 (b :+ sb) = firstHitAt' q p ss'
                 ae        = valOf a sa
             in case (a /= b, a == p) of
                  (True, _)     -> [ b :+ Right (a :+ ae,sb)
                                   , a :+ Left  ae -- a must be a vertex!
                                   ] -- new window of the output polygon discovered
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
                               $ supportingLine (s^.core) `intersect`lineThrough p q

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
                      Nothing -> error "firstHitAt: precondition failed!"

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
deleteAt     :: (Ord r, Fractional r, Show r, Show p)
             => Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
deleteAt p q = Set.deleteAllBy (compareByDistanceToAt q p <> compareAroundEndPoint)
  -- if two segments have the same distance, we use the ccw order around their common
  -- (end) point.

-- FIXME: If there are somehow segmetns that would continue at p as
-- well, they are also deleted.


-- | Given a point q compute the subset of segments intersecting the
-- horizontal rightward ray starting in q, and order them by
-- increasing dsitance.
fromListByDistTo   :: forall r p e. (Ord r, Fractional r)
                   => Point 2 r -> [LineSegment 2 p r :+ e] -> Status p e r
fromListByDistTo q = Set.mapMonotonic (^.extra)
                   . foldr (Set.insertBy $ comparing (^.core)) Set.empty
                   . mapMaybe (initialIntersection q)

-- | Given q and a segment s, computes if the segment intersects the initial, rightward
-- ray starting in q, and if so returns the (squared) distance from q to that point
-- together with the segment
initialIntersection     :: forall r p e. (Ord r, Fractional r)
                        => Point 2 r -> LineSegment 2 p r :+ e
                        -> Maybe (r :+ (LineSegment 2 p r :+ e))
initialIntersection q s =
  case asA @(Point 2 r) $ (s^.core) `intersect` horizontalLine (q^.yCoord) of
    Nothing -> Nothing
    Just p  -> let d = p^.xCoord - q^.xCoord
               in if d < 0 then Nothing else Just (d :+ s)
-- TODO: use the faster test intersecting things with a horizontalLine



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
compareAroundEndPoint (LineSegment' p q :+ _)
                      (LineSegment' s t :+ _)
    | p^.core == s^.core = ccwCmpAround p q t
    | p^.core == t^.core = ccwCmpAround p q s
    | q^.core == s^.core = ccwCmpAround q p t
    | q^.core == t^.core = ccwCmpAround q p s
    | otherwise          = error "compareAroundEndPoint: precondition failed!"


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
