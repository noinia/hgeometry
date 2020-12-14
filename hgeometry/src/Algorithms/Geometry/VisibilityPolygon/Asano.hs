module Algorithms.Geometry.VisibilityPolygon.Asano where

import           Control.Lens
import           Control.Monad ((<=<))
import           Data.Ext
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Intersection
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Set.Util as Set
import           Data.Vinyl.CoRec

--------------------------------------------------------------------------------

type StarShapedPolygon p r = SimplePolygon p r


data EventKind = Insert | Delete deriving (Show,Eq,Ord)

data Event p e r = Event { _eventVtx :: Point 2 r :+ p
                         , _toInsert :: [LineSegment 2 p r :+ e]
                         , _toDelete :: [LineSegment 2 p r :+ e]
                         } deriving Show

type Status p e r = Set.Set (LineSegment 2 p r :+ e)


-- | Vertices of the visibility polgyon are either original vertices
-- or defined by some vertex and an edge
type Definer p e r = Either p (Point 2 r :+ p,LineSegment 2 p r :+ e)


--------------------------------------------------------------------------------

-- | pre: - all line segments are considered closed.
--        - no singleton linesegments exactly pointing away from q.
visibilityPolygon'        :: forall p r e. (Ord r, Fractional r)
                          => Point 2 r
                          -> [LineSegment 2 p r :+ e]
                          -> StarShapedPolygon (Definer p e r) r
visibilityPolygon' q segs = fromPoints . snd
                          $ List.foldl' (handleEvent q) (statusStruct,[]) events
  where
    statusStruct = fromListByDistTo q segs

    events    :: [Event p e r]
    events    = map (mkEvent q)
              . groupBy'    (ccwCmpAround (ext q))
              . List.sortBy (ccwCmpAround (ext q))
              $ endPoints'

    endPoints' = concatMap (\s@(LineSegment' u v :+ _) -> [ u&extra %~ (,v,s)
                                                          , v&extra %~ (,u,s)
                                                          ]
                           ) segs


mkEvent               :: (Ord r, Num r)
                      => Point 2 r
                      -> NonEmpty (Point 2 r :+ (p, Point 2 r :+ p, LineSegment 2 p r :+ e))
                      -> Event p e r
mkEvent q ps@(p :| _) = Event (p&extra %~ \(e,_,_) -> e) (f <$> ins) (f <$>  dels)
  where
    (ins,_colinears,dels) = partition3 (\(u :+ (_,v,_)) -> ccwCmpAround (ext q) (ext u) v) ps
    f (_ :+ (_,_,s)) = s


-- | Handles an event, computes the new status structure and output polygon.
handleEvent                                     :: (Ord r, Fractional r)
                                                => Point 2 r
                                                -> (Status p e r, [Point 2 r :+ Definer p e r])
                                                -> Event p e r
                                                -> (Status p e r, [Point 2 r :+ Definer p e r])
handleEvent q (ss,out) (Event (p :+ z) is dels) = (ss', newVtx <> out)
  where
    ss' = flip (foldr (insertAt q p)) is
        . flip (foldr (deleteAt q p)) dels
        $ ss

    newVtx = let (a :+ sa) = firstHitAt' q p ss
                 (b :+ sb) = firstHitAt' q p ss'
                 ae        = valOf a sa
             in case (a /= b, a == p) of
                  (True, _)     -> [ a :+ Left  ae -- a must be a vertex!
                                   , b :+ Right (a :+ ae,sb)
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
firstHitAt     :: forall p r e. (Ord r, Fractional r)
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
insertAt q p = Set.insertBy (compareByDistanceToAt q p)

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
deleteAt p q = Set.deleteAllBy (compareByDistanceToAt q p)

-- | Given a point q compute the subset of segments intersecting the
-- horizontal rightward ray starting in q, and order them by
-- increasing dsitance.
fromListByDistTo   :: forall r p e. (Ord r, Fractional r)
                   => Point 2 r -> [LineSegment 2 p r :+ e] -> Status p e r
fromListByDistTo q = foldr (Set.insertBy cmp) Set.empty
  where
    cmp        = comparing f
    f (s :+ _) = case asA @(Point 2 r) $ s `intersect` (horizontalLine $ q^.yCoord) of
                   Nothing -> Nothing
                   Just p  -> let d = p^.xCoord - q^.xCoord
                              in if d < 0 then Nothing else Just d
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
