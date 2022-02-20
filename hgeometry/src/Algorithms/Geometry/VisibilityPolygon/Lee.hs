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
module Algorithms.Geometry.VisibilityPolygon.Lee
  ( visibilityPolygon
  , visibilitySweep
  , VisibilityPolygon
  , Definer, StarShapedPolygon
  , compareAroundEndPoint
  ) where

import           Algorithms.Geometry.RayShooting.Naive
import           Control.Lens
import           Control.Monad ((<=<))
import           Data.Bifunctor (first)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
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
import qualified Data.List.Util as List
import           Data.Maybe (mapMaybe, isJust)
import           Data.Ord (comparing)
import           Data.RealNumber.Rational
import           Data.Semigroup.Foldable
import qualified Data.Set as Set
import qualified Data.Set.Util as Set
import           Data.Util
import           Data.Vinyl.CoRec
import           Debug.Trace

type R = RealNumber 5

--------------------------------------------------------------------------------

type StarShapedPolygon p r = SimplePolygon p r

-- | Vertices of the visibility polgyon are either original vertices
-- or defined by some vertex and an edge
type Definer p e r = Either p (Point 2 r :+ p,LineSegment 2 p r :+ e)

type VisibilityPolygon p e r = StarShapedPolygon (Definer p e r) r

-- | We either insert or delete segments
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



--------------------------------------------------------------------------------




-- | Computes the visibility polygon of a point q in a polygon with
-- \(n\) vertices.
--
-- pre: q lies strictly inside the polygon
--
-- running time: \(O(n\log n)\)
visibilityPolygon      :: forall p t r. (Ord r, Fractional r)
                       => Point 2 r
                       -> Polygon t p r
                       -> StarShapedPolygon (Definer p () r) r
visibilityPolygon q pg =
    fromPoints . visibilitySweep v Nothing q . map ext . closedEdges $ pg
  where
    v = uncurry (startingDirection q) . consecutive q . polygonVertices $ pg























-- | Computes the visibility polgyon from a vertex
visibilityPolygonFromVertex      :: forall p t r. (Ord r, Fractional r, Show r, Show p)
                                 => Polygon t p r
                                 -> Int -- ^ from the i^th vertex on the outer boundary
                                 -> VisibilityPolygon p () r
visibilityPolygonFromVertex pg i =
    fromPoints . visibilitySweep sv (Just w) v . map ext $ segs
  where
    (v :+ _) = pg^.outerVertex i
    (w :+ _) = pg^.outerVertex (i-1)
    (u :+ _)  = pg^.outerVertex (i+1)

    -- rotates the polygon so that u becomes the focus, and gets all
    -- other vertices. Takes the next CCW vertex around v, starting
    -- form the direction indicated by v.
    z = let u' :| rest = traceShowIdWith "vertices"
                       $  polygonVertices $ pg&outerBoundary %~ rotateRight (i+1)
        in traceShowIdWith "z" $ consecutiveFrom (u .-. v) v (List.init rest)
           -- the last vertex in rest is v; so kill that

    sv = startingDirection v u z

    segs = map (first (^._2))
         . filter (not . incidentTo i)
         . closedEdges $ numberVertices pg

visibilityPolygonFromVertex' q sv mt segs = sweep q statusStruct (traceShowIdWith "events" events)
  where
    v      = undefined

    -- lazily test if the segment intersects the initial ray
    segs'  = labelWithDistances q initialRay segs

    events = computeEvents q sv (untilEnd q sv mt) segs'
        -- take only until the end of the range (if defined)

    initialRay = traceShowIdWith "ray" $ HalfLine q sv
    statusStruct = traceShowIdWith "initialSS" $ mkInitialSS segs'


-- | Test if the line segment is incident to a point with the given
-- index.
incidentTo     :: Int -> LineSegment 2 (SP Int a) r -> Bool
incidentTo i s = s^.start.extra._1 == i || s^.end.extra._1 == i









-- | computes a (partial) visibility polygon of a set of \(n\)
-- disjoint segments. The input segments are allowed to share
-- endpoints, but no intersections or no endpoints in the interior of
-- other segments. The input vector indicates the starting direction,
-- the Maybe point indicates up to which point/dicrection (CCW) of the
-- starting vector we should compute the visibility polygon.
--
-- pre : - all line segments are considered closed.
--       - no singleton linesegments exactly pointing away from q.
--       - for every orientattion the visibility is blocked somewhere, i.e.
--            no rays starting in the query point q that are disjoint from all segments.
--       - no vertices at staring direction sv
--
-- running time: \(O(n\log n)\)
visibilitySweep              :: forall p r e. (Ord r, Fractional r)
                             => Vector 2 r -- ^ starting direction of the sweep
                             -> Maybe (Point 2 r)
                             -- ^ -- point indicating the last point to sweep to
                             -> Point 2 r -- ^ the point form which we compute the visibility polgyon
                             -> [LineSegment 2 p r :+ e]
                             -> [Point 2 r :+ Definer p e r]
visibilitySweep sv mt q segs = sweep q statusStruct events
  where
    -- lazily test if the segment intersects the initial ray
    segs'  = labelWithDistances q initialRay segs
    events = computeEvents q sv (untilEnd q sv mt) segs'

    initialRay = HalfLine q sv
    statusStruct = mkInitialSS segs'

-- | Take until the ending point if defined. We can use that the list
-- of events appears in sorted order in the cyclic orientation around
-- the query point q
untilEnd      :: (Ord r, Num r)
              => Point 2 r -- ^ query point
              -> Vector 2 r -- ^ starting direction
              -> Maybe (Point 2 r) -- ^ possible ending point
              -> [Event a e r] -> [Event a e r]
untilEnd q sv = \case
  Nothing -> id
  Just t  -> List.takeWhile (\e -> ccwCmpAroundWith' sv (ext q) (e^.eventVtx) (ext t) == LT)

-- | Runs the actual sweep
sweep                :: (Foldable t, Ord r, Fractional r)
                     => Point 2 r    -- ^ query point
                     -> Status p e r -- ^ initial status structure
                     -> t (Event p e r) -- ^ events to handle
                     -> [Point 2 r :+ Definer p e r]
sweep q statusStruct = snd . List.foldl' (handleEvent q) (statusStruct,[])


-- | Computes the events in the sweep
computeEvents                :: (Ord r, Num r, Foldable t)
                             => Point 2 r -- ^ query point
                             -> Vector 2 r -- ^ starting direction
                             -> ([Event p1 e1 r] -> [Event p2 e2 r]) -- ^ until where to take the vents
                             -> t (LineSegment 2 p1 r :+ (Maybe r, e1))
                             -> [Event p2 e2 r]
computeEvents q sv takeUntil =
     map (combine q)
   . List.groupBy' (\a b -> ccwCmpAroundWith' sv (ext q) (a^.eventVtx) (b^.eventVtx))
   . takeUntil
   . List.sortBy (cmp `on` (^.eventVtx))
   . concatMap (mkEvent sv q)
  where
    cmp = ccwCmpAroundWith' sv (ext q) <> cmpByDistanceTo' (ext q)

-- | Given multiple events happening at the same orientation, combine
-- them into a single event.
combine      :: (Ord r, Num r) => Point 2 r -> NonEmpty (Event p e r) -> Event p e r
combine q es = Event p acts
  where
    acts = foldMap1 (^.actions) es
    p    = F.minimumBy (cmpByDistanceTo' (ext q)) . fmap (^.eventVtx) $ es

-- | Constructs the at most two events resulting from this segement.
mkEvent                                      :: (Ord r, Num r)
                                             => Vector 2 r -- ^ starting direction
                                             -> Point 2 r  -- ^ query point
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


-- | Handles an event, computes the new status structure and output polygon.
handleEvent                                  :: (Ord r, Fractional r)
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
firstHitAt     :: forall p r e. (Ord r, Fractional r)
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
firstHitAt'        :: forall p r e. (Ord r, Fractional r)
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
insertAt     :: (Ord r, Fractional r)
             => Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
insertAt q p = Set.insertBy (compareByDistanceToAt q p <> flip (compareAroundEndPoint q))
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
deleteAt     :: (Ord r, Fractional r)
             => Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
deleteAt q p = Set.deleteAllBy (compareByDistanceToAt q p <> compareAroundEndPoint q)
  -- if two segments have the same distance, we use the ccw order around their common
  -- (end) point.

-- FIXME: If there are somehow segmetns that would continue at p as
-- well, they are also deleted.


-- | Given a list of line segments, each labeled with the distance
-- from their intersection point with the initial ray to the query
-- point, build the initial status structure.
mkInitialSS :: forall r p e. (Ord r, Fractional r)
            => [ LineSegment 2 p r :+ (Maybe r, e)] -> Status p e r
mkInitialSS = Set.mapMonotonic (^.extra)
            . foldr (Set.insertBy $ comparing (^.core)) Set.empty
            . mapMaybe (\(s :+ (md,e)) -> (:+ (s :+ e)) <$> md)

-- | Given q, the initial ray, and a segment s, computes if the
-- segment intersects the initial, rightward ray starting in q, and if
-- so returns the (squared) distance from q to that point together
-- with the segment.
initialIntersection         :: forall r p. (Ord r, Fractional r)
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
-- counterclockwise order around u (treating the direction from q to
-- the common endpoint as zero).
compareAroundEndPoint  :: forall p r e. (Ord r, Fractional r)
                       => Point 2 r
                       -> LineSegment 2 p r :+ e
                       -> LineSegment 2 p r :+ e
                       -> Ordering
compareAroundEndPoint q
                      (LineSegment' a b :+ _)
                      (LineSegment' s t :+ _)
    -- traceshow ("comapreAroundEndPoint ", sa, sb) False = undefined
    | a^.core == s^.core = ccwCmpAroundWith' (a^.core .-. q) a b t
    | a^.core == t^.core = ccwCmpAroundWith' (a^.core .-. q) a b s
    | b^.core == s^.core = ccwCmpAroundWith' (b^.core .-. q) b a t
    | b^.core == t^.core = ccwCmpAroundWith' (b^.core .-. q) b a s
    | otherwise          = error "compareAroundEndPoint: precondition failed!"

--------------------------------------------------------------------------------
-- * Helper functions for polygon operations

-- | Given q, and two consecutive points u and v, Computes a direction
-- for the initial ray, i.e. a "generic" ray that does not go through
-- any vertices.
startingDirection       :: Fractional r => Point 2 r -> Point 2 r -> Point 2 r -> Vector 2 r
startingDirection q u w = v .-. q
  where
    v = u .+^ ((w .-. u) ^/ 2) -- point in the middle between u and w
        -- note: the segment between u and w could pass on the wrong side of q
        -- (i.e. so that does not "cover" the CCW but the CW range between u and w)
        -- however, in that case there is apparently nothing on the CCW side opposite
        -- to v, as u and w are supposed to be the first two events. This means the
        -- precondition does not hold.

-- | finds two consecutive vertices in the clockwise order around the
-- given point q. I.e. there are no other points in between the two
-- returned points.
consecutive                   :: (Ord r, Num r) => Point 2 r -> NonEmpty (Point 2 r :+ p)
                              -> (Point 2 r, Point 2 r)
consecutive q ((p :+ _):|pts) = (p,consecutiveFrom (p .-. q) q pts)

-- | pre: input list is non-empty
consecutiveFrom     :: (Ord r, Num r)
                    => Vector 2 r -- ^ starting vector
                    -> Point 2 r -- ^ query point
                    -> [Point 2 r :+ p] -> Point 2 r
consecutiveFrom v q = view core . List.minimumBy (ccwCmpAroundWith' v (ext q))

-- | Gets the edges of the polygon as closed line segments.
closedEdges :: Polygon t p r -> [LineSegment 2 p r]
closedEdges = map asClosed . listEdges
  where
    asClosed (LineSegment' u v) = ClosedLineSegment u v


--------------------------------------------------------------------------------
-- * Generic Helper functions



--------------------------------------------------------------------------------

test :: StarShapedPolygon (Definer Int () R) R
test = visibilityPolygon origin testPg

testVtx = visibilityPolygonFromVertex testPg 0

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



traceShowIdWith x y = traceShow (show x,y) y
