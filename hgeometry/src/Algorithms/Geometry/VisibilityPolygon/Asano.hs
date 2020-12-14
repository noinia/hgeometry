module Algorithms.Geometry.VisibilityPolygon.Asano where

import           Control.Lens
import           Control.Monad ((<=<))
import           Data.Ext
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
type Definer p e = Either p (p,e)


-- | pre: - all line segments are considered closed.
--        - no singleton linesegments exactly pointing away from q.
visibilityPolygon'        :: (Ord r, Fractional r)
                          => Point 2 r
                          -> [LineSegment 2 p r :+ e]
                          -> StarShapedPolygon (Definer p e) r
visibilityPolygon' q segs = fromPoints . snd $ List.foldl' handleEvent (statusStruct,[]) events
  where
    initialRay = HalfLine q (Vector2 1 0)
    statusStruct = fromListByDistTo q $ filter (\(s :+ _) -> initialRay `intersects` s) segs
                   -- FIXME: just pass the ray, we are computing intersection points twice now

    events    = map (mkEvent q)
              . NonEmpty.groupWith (ccwCmpAround (ext q))
              . List.sortBy        (ccwCmpAround (ext q))
              $ endPoints'

    endPoints' = concatMap (\s@(LineSegment' u v :+ _) -> [ u&extra %~ (,v,s)
                                                          , v&extra %~ (,u,s)
                                                          ]
                           ) segs


handleEvent                                     :: (Ord r, Fractional r)
                                                => Point 2 r
                                                -> (Status p e r, [Point 2 r :+ Definer p e])
                                                -> Event p e r
                                                -> (Status p e r, [Point 2 r :+ Definer p e])
handleEvent q (ss,out) (Event (p :+ z) is dels) = (ss', newVtx <> out)
  where
    ss' = flip (foldr (insertAt q p)) is
        . flip (foldr (deleteAt q p)) dels $ ss

    newVtx = let a = firstHitAt q p ss
                 b = firstHitAt q p ss'
             in case (a /= b, a == p) of
                  (True, _)     -> [a&extra %~ Left
                                   ,b&extra %~ Right (a^.core,)
                    ] -- new window of the output polygon discovered
                  (False,True)  -> [Left $ p :+ z]
                    -- sweeping over a regular vertex of the visibility polygon
                  (False,False) -> []    -- sweeping over a vertex not in output


partition3   :: Foldable f => (a -> Ordering) -> f a -> ([a],[a],[a])
partition3 f = foldr g ([],[],[])
  where
    g x (lts,eqs,gts) = case f x of
                          LT -> (x:lts,eqs  ,gts)
                          EQ -> (lts,  x:eqs,gts)
                          GT -> (lts,  eqs  ,x:gts)

mkEvent               :: (Ord r, Num r)
                      => Point 2 r
                      -> NonEmpty (Point 2 r :+ (p, Point 2 r :+ p, LineSegment 2 p r :+ e))
                      -> Event p e r
mkEvent q ps@(p :| _) = Event (p&extra %~ \(e,_,_) -> e) (f <$> ins) (f <$>  dels)
  where
    (ins,_colinears,dels) = partition3 (\(u :+ (_,v,_)) -> ccwCmpAround (ext q) (ext u) v) ps
    f (_ :+ (_,_,s)) = s


-- | Given two points q and p, and a status structure
-- retrieve the first segment in the status structure intersected
-- by the ray from q through p.
--
-- pre: all segments in the status structure should intersect the ray
--      from q through p, in that order.
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

-- | Insert a new segment into the status structure, depending on the
-- (distance from q to to the) intersection point with the ray from q
-- through p
--
-- pre: all segments in the status structure should intersect the ray
--      from q through p, in that order.
--
-- \(O(\log n)\)
insertAt     :: Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
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
deleteAt     :: Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
deleteAt p q = Set.deleteAllBy (compareByDistanceToAt q p)

-- |
fromListByDistTo   :: Point 2 r -> [LineSegment 2 p r :+ e] -> Status p e r
fromListByDistTo q = let cmp = compareByDistanceToAt q (q .+^ Vector2 1 0)
                     in foldr (Set.insertBy cmp) mempty
                        -- TODO: use the faster test intersecting things with a horizontalLine
