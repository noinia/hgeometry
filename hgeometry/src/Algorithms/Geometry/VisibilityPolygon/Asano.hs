module Algorithms.Geometry.VisibilityPolygon.Asano where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Set.Util as Set

--------------------------------------------------------------------------------

type StarShapedPolygon p r = SimplePolygon p r


data EventKind = Insert | Delete deriving (Show,Eq,Ord)

data Event p e r = Event { _eventVtx :: Point 2 r :+ p
                         , _toInsert :: [LineSegment 2 p r :+ e]
                         , _toDelete :: [LineSegment 2 p r :+ e]
                         } deriving Show

type Status p e r = Set.Set (LineSegment 2 p r :+ e)


-- |
visibilityPolygon'        :: Point 2 r
                          -> [LineSegment 2 p r :+ e]
                          -> StarShapedPolygon () r
visibilityPolygon' q segs = fromPoints . snd $ List.foldl' handleEvent (statusStruct,[]) events
  where
    initialRay = undefined
    statusStruct = fromListByDistTo q $ filter (`intersects` initialRay) segs
    events    = map (mkEvent q)
              . mapMaybe NonEmpty.nonEmpty
              . List.groupBy (ccwCmpAround q)
              . List.sortBy (ccwCmpAround q)
              $ endPoints

    endPoints = concatMap (\s@(LineSegment' u v :+ e) -> [u :+ s, v :+ s]) segs


handleEvent                              :: (e ~ ())
                                         => Point 2 r
                                         -> (Status p e r,[Point 2 r :+ p]) -> Event p e r
                                         -> (Status p e r,[Point 2 r :+ p])
handleEvent q (ss,out) (Event p is dels) = (ss', newVtx <> out)
  where
    ss' = flip (foldr (insertAt q p)) is
        . flip (foldr (deleteAt q p)) dels $ ss

    newVtx = let a = firstHitAt q p ss
                 b = firstHitAt q p ss'
             in case (a /= b, a == p) of
                  (True, _)     -> [a,b] -- new window of the output discovered
                  (False,True)  -> [p]   -- sweeping over a regular vertex of the visibility polygon
                  (False,False) -> []    -- sweeping over a vertex not in output


mkEvent               :: Point 2 r
                      -> NonEmpty ((Point 2 r :+ p) :+ (LineSegment 2 p r :+ e))
                      -> Event p e r
mkEvent q ps@(p :| _) = Event (p^.core) ins dels
  where
    (ins,dels) = undefined -- partition ps

firstHitAt     :: Point 2 r -> Point 2 r -> Status p e r
               -> Maybe (Point 2 r :+ LineSegment 2 p e)
firstHitAt q p = computeIntersectionPoint <=< Set.lookupMin
  where
    computeIntersectionPoint = undefined

compareByDistanceToAt           :: Point 2 r -> Point 2 r
                                -> LineSegment 2 p r :+ e
                                -> LineSegment 2 p r :+ e
                                -> Ordering
compareByDistanceToAt q p sa sb = undefined
  where
    v = p .-. q

insertAt     :: Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
insertAt q p = Set.insertBy (compareByDistanceToAt q p)

deleteAt     :: Point 2 r -> Point 2 r -> LineSegment 2 p r :+ e
             -> Status p e r -> Status p e r
deleteAt p q = Set.deleteAllBy (compareByDistanceToAt q p)

fromListByDistTo   :: Point 2 r -> [LineSegment 2 p r :+ e] -> Status p e r
fromListByDistTo q = undefined
