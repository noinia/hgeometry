{-# Language TemplateHaskell #-}
module Algorithms.Geometry.VerticalRayShooting.PersistentSweep
  ( VerticalRayShootingStructure(VerticalRayShootingStructure), StatusStructure
  , leftMost, sweepStruct

  -- * Building the Data Structure
  , verticalRayShootingStructure
  -- * Querying the Data Structure
  , segmentAbove

  , searchInSlab
  , ordAt, yCoordAt
  ) where

import           Algorithms.BinarySearch (binarySearchVec)
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable
import qualified Data.Set as SS -- status struct
import qualified Data.Set.Util as SS
import qualified Data.Vector as V


--------------------------------------------------------------------------------

-- | The vertical ray shooting data structure
data VerticalRayShootingStructure p e r =
    VerticalRayShootingStructure { _leftMost    :: r
                                 , _sweepStruct :: V.Vector (r :+ StatusStructure p e r)
                                   -- ^ entry (r :+ s) means that "just" left of "r" the
                                   -- status structure is 's', i.e up to 'r'
                                 }
  deriving (Show,Eq)

type StatusStructure p e r = SS.Set (LineSegment 2 p r :+ e)

makeLensesWith (lensRules&generateUpdateableOptics .~ False) ''VerticalRayShootingStructure

--------------------------------------------------------------------------------
-- * Building the DS


-- | Given a set of \(n\) interiorly pairwise disjoint segments,
-- compute a vertical ray shooting data structure.  (i.e. the
-- endpoints of the segments may coincide).
--
-- pre: no vertical segments (they are ignored)
--
-- running time: \(O(n\log n)\).
-- space: \(O(n\log n)\).
verticalRayShootingStructure   :: (Ord r, Fractional r, Foldable1 t)
                               => t (LineSegment 2 p r :+ e)
                               -> VerticalRayShootingStructure p e r
verticalRayShootingStructure ss = VerticalRayShootingStructure (eventX e) (sweep' events)
  where
    events@(e :| _) = fmap combine
                    . NonEmpty.groupAllWith1 eventX
                    . foldMap1 toEvents
                    $ ss
    sweep' = V.fromList . toList . sweep

-- | Given a bunch of events happening at the same time, merge them into a single event
-- where we apply all actions.
combine                    :: NonEmpty (Event p e r) -> Event p e r
combine es@((x :+ _) :| _) = x :+ foldMap1 eventActions es

-- | Given a line segment construct the two events; i.e. when we
-- insert it and when we delete it.
toEvents   :: Ord r => LineSegment 2 p r :+ e -> NonEmpty (Event p e r)
toEvents s = let (p,q) = bimap (^.core) (^.core) . orderedEndPoints $ s^.core
             in NonEmpty.fromList [ (p^.xCoord) :+ Insert s :| []
                                  , (q^.xCoord) :+ Delete s :| []
                                  ]

----------------------------------------

data Action a = Insert a | Delete a  deriving (Show,Eq)

interpret :: Action a -> (a -> a -> Ordering) -> SS.Set a -> SS.Set a
interpret = \case
  Insert s -> \cmp -> SS.insertBy    cmp s
  Delete s -> \cmp -> SS.deleteAllBy cmp s

type Event p e r = r :+ NonEmpty (Action (LineSegment 2 p r :+ e))

eventX :: Event p e r -> r
eventX = view core

eventActions :: Event p e r -> NonEmpty (Action (LineSegment 2 p r :+ e))
eventActions = view extra

----------------------------------------

-- | Runs the sweep building the data structure from left to right.
sweep    :: (Ord r, Fractional r)
         => NonEmpty (Event p e r) -> NonEmpty (r :+ StatusStructure p e r)
sweep es = NonEmpty.fromList
         . snd . List.mapAccumL h SS.empty
         $ zip (toList es) (NonEmpty.tail es)
  where
    h ss evts = let x :+ ss' = handle ss evts in (ss',x :+ ss')

-- TODO: Verify that mapAccumL does not leak memory like foldl does.

-- | Given the current status structure (for left of the next event
-- 'l'), and the next two events (l,r); essentially defining the slab
-- between l and r, we construct the status structure for in the slab (l,r).
-- returns the right boundary and this status structure.
handle                :: (Ord r, Fractional r)
                      => StatusStructure p e r
                      -> (Event p e r, Event p e r)
                      -> r :+ StatusStructure p e r
handle ss ( l :+ acts
          , r :+ _)   = let mid               = (l+r)/2
                            runActionAt x act = interpret act (ordAt x)
                        in r :+ foldr (runActionAt mid) ss acts

--------------------------------------------------------------------------------
-- * Querying the DS


-- | Find the segment vertically strictly above query point q, if it exists.
--
-- \(O(\log n)\)
segmentAbove :: Ord r
             => Point 2 r -> VerticalRayShootingStructure p e r -> Maybe (LineSegment 2 p r :+ e)
segmentAbove q ds | q^.xCoord < ds^.leftMost = Nothing
                  | otherwise                = binarySearchVec (q `leftOf `) (ds^.sweepStruct)
                                               >>= undefined -- searchInSlab q
  where
    q `leftOf` (r :+ _) = q^.xCoord <= r


-- | Find the segment vertically query point q, if it exists.
--
-- \(O(\log n)\)
segmentOnOrAbove  :: Ord r
                  => Point 2 r -> VerticalRayShootingStructure p e r
                  -> Maybe (LineSegment 2 p r :+ e)
segmentOnOrAbove = undefined
-- Maybe just expose a version that you can pass lookupGE, lookupGT, etc. to


-- | Finds the first segment directly above q
searchInSlab      :: Ord r
                  => Point 2 r -> StatusStructure p e r -> Maybe (LineSegment 2 p r :+ e)
searchInSlab q ss = let qs = ClosedLineSegment (q :+ undefined) (q :+ undefined)
                    in undefined -- SS.lookupGT qs ss


  -- mi >>= \i -> vss^?ix i.extra >>= successorOf qy
  -- where
  --   vss   = ds^.sweepStruct
  --   -- the index of the slab containing the query point (if such a slab exists)
  --   mi = binarySearchVec (\(x' :+ _) -> x' > ValB qx) vss
  --   -- the successor in the status-struct corresponding to the current slab
  --   successorOf y ss = let (_,m,r) = SS.splitOn (yCoordAt' (ds^.subdivision) qx) y ss
  --                      in SS.lookupMin $ m <> r


-- -- | General query lifting function
-- queryWith                   :: (r -> r -> Ordering) -> (q -> r) -> (a -> r)
--                             -> (a -> Set a -> b)
--                             -> q -> SS.Set a -> b
-- queryWith cmp f g query q s = withOrd cmp $ liftOrd1 (query (f q)) s


----------------------------------------------------------------------------------

type Compare a = a -> a -> Ordering

-- | Compare based on the y-coordinate of the intersection with the horizontal
-- line through y
ordAt   :: (Fractional r, Ord r) => r -> Compare (LineSegment 2 p r :+ e)
ordAt x = comparing (yCoordAt x)


-- | Given an x-coordinate and a line segment that intersects the vertical line
-- through x, compute the y-coordinate of this intersection point.
--
-- note that we will pretend that the line segment is closed, even if it is not
yCoordAt :: (Fractional r, Ord r) => r -> LineSegment 2 p r :+ e -> r
yCoordAt x (LineSegment' (Point2 px py :+ _) (Point2 qx qy :+ _) :+ _)
    | px == qx  = py `max` qy -- s is vertical, since by the precondition it
                              -- intersects we return the y-coord of the topmost
                              -- endpoint.
    | otherwise = py + alpha * (qy - py)
  where
    alpha = (x - px) / (qx - px)

--------------------------------------------------------------------------------
