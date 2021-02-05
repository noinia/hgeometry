{-# Language TemplateHaskell #-}
module Algorithms.Geometry.VerticalRayShooting.PersistentSweep
  ( VerticalRayShootingStructure(VerticalRayShootingStructure), StatusStructure
  , leftMost, sweepStruct

  -- * Building the Data Structure
  , verticalRayShootingStructure
  -- * Querying the Data Structure
  , segmentAbove, segmentAboveOrOn
  , findSlab
  , lookupAbove, lookupAboveOrOn, searchInSlab
  , ordAt, yCoordAt
  ) where

import           Algorithms.BinarySearch (binarySearchIn)
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable
import qualified Data.Set as SS -- status struct
import qualified Data.Set.Util as SS
import qualified Data.Vector as V


import           Data.RealNumber.Rational

type R = RealNumber 5
--------------------------------------------------------------------------------

-- | The vertical ray shooting data structure
data VerticalRayShootingStructure p e r =
    VerticalRayShootingStructure { _leftMost    :: r
                                 , _sweepStruct :: V.Vector (r :+ StatusStructure p e r)
                                   -- ^ entry (r :+ s) means that "just" left of "r" the
                                   -- status structure is 's', i.e up to 'r'
                                 } deriving (Show,Eq)

type StatusStructure p e r = SS.Set (LineSegment 2 p r :+ e)

makeLensesWith (lensRules&generateUpdateableOptics .~ False) ''VerticalRayShootingStructure

--------------------------------------------------------------------------------
-- * Building the DS

-- | Given a set of \(n\) interiorly pairwise disjoint *closed* segments,
-- compute a vertical ray shooting data structure.  (i.e. the
-- endpoints of the segments may coincide).
--
-- pre: no vertical segments
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
                    . NonEmpty.fromList -- precondition guarantees that this is safe
                    . mapMaybe reOrient . toList
                    $ ss
    sweep' = V.fromList . toList . sweep

    reOrient s'@(s :+ z) = case (s^.start.core.xCoord) `compare` (s^.end.core.xCoord) of
                             LT -> Just s'
                             GT -> let s'' = s&start .~ (s^.end) -- flip the segment
                                              &end   .~ (s^.start)
                                   in Just $ s'' :+ z
                             EQ -> Nothing -- precondition says this won't happen, but kill
                                           -- them anyway

-- | Given a bunch of events happening at the same time, merge them into a single event
-- where we apply all actions.
combine                    :: NonEmpty (Event p e r) -> Event p e r
combine es@((x :+ _) :| _) = x :+ foldMap1 eventActions es

-- | Given a line segment construct the two events; i.e. when we
-- insert it and when we delete it.
toEvents                           :: Ord r => LineSegment 2 p r :+ e -> NonEmpty (Event p e r)
toEvents s@(LineSegment' p q :+ _) = NonEmpty.fromList [ (p^.core.xCoord) :+ Insert s :| []
                                                       , (q^.core.xCoord) :+ Delete s :| []
                                                       ]

----------------------------------------

data Action a = Insert a | Delete a  deriving (Show,Eq)

{- HLINT ignore "Avoid lambda using `infix`" -}
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
                        in r :+ foldr (runActionAt mid) ss (orderActs acts)
                           -- run deletions first

-- | orders the actions to put insertions first and then all deletions
orderActs      :: NonEmpty (Action a) -> NonEmpty (Action a)
orderActs acts = let (dels,ins) = NonEmpty.partition (\case
                                                         Delete _ -> True
                                                         Insert _ -> False
                                                     ) acts
                 in NonEmpty.fromList $ ins <> dels


--------------------------------------------------------------------------------
-- * Querying the DS

-- | Find the segment vertically strictly above query point q, if it
-- exists.
--
-- \(O(\log n)\)
segmentAbove      :: (Ord r, Num r) => Point 2 r -> VerticalRayShootingStructure p e r
                  -> Maybe (LineSegment 2 p r :+ e)
segmentAbove q ds = findSlab q ds >>= lookupAbove q

-- | Find the segment vertically query point q, if it exists.
--
-- \(O(\log n)\)
segmentAboveOrOn      :: (Ord r, Num r)
                      => Point 2 r -> VerticalRayShootingStructure p e r
                      -> Maybe (LineSegment 2 p r :+ e)
segmentAboveOrOn q ds = findSlab q ds >>= lookupAboveOrOn q



-- | Given a query point, find the (data structure of the) slab containing the query point
--
-- \(O(\log n)\)
findSlab :: Ord r
         => Point 2 r -> VerticalRayShootingStructure p e r -> Maybe (StatusStructure p e r)
findSlab q ds | q^.xCoord < ds^.leftMost = Nothing
              | otherwise                = view extra
                                        <$> binarySearchIn (q `leftOf `) (ds^.sweepStruct)
  where
    q' `leftOf` (r :+ _) = q'^.xCoord <= r

--------------------------------------------------------------------------------
-- * Querying in a single slab

-- | Finds the segment containing or above the query point 'q'
--
-- \(O(\log n)\)
lookupAboveOrOn   :: (Ord r, Num r)
                  => Point 2 r -> StatusStructure p e r -> Maybe (LineSegment 2 p r :+ e)
lookupAboveOrOn q = searchInSlab (not . (q `liesAbove`))

-- | Finds the first segment strictly above q
--
-- \(O(\log n)\)
lookupAbove   :: (Ord r, Num r)
              => Point 2 r -> StatusStructure p e r -> Maybe (LineSegment 2 p r :+ e)
lookupAbove q = searchInSlab (q `liesBelow`)

-- | generic searching function
searchInSlab   :: Num r => (Line 2 r -> Bool)
               -> StatusStructure p e r -> Maybe (LineSegment 2 p r :+ e)
searchInSlab p = binarySearchIn (p . supportingLine . view core)


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

test1 :: VerticalRayShootingStructure () Int R
test1 = verticalRayShootingStructure . NonEmpty.fromList $ zipWith (:+)
        [ hor 2 0 10
        , hor 4 1 12
        , hor 2 10 14
        ] [1..]
  where
    hor y l r = ClosedLineSegment (ext $ Point2 l y) (ext $ Point2 r y)


-- shouldBe = (==)

-- foo = segmentAbove (Point2 5 0) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
-- foo = segmentAbove (Point2 5 2) test1 `shouldBe` Just (LineSegment (Closed (Point2 1 4 :+ ())) (Closed (Point2 12 4 :+ ())) :+ 2)
-- foo = segmentAbove (Point2 5 1) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
-- foo = segmentAbove (Point2 5 5) test1 `shouldBe` Nothing
-- foo = segmentAbove (Point2 10 5) test1 `shouldBe` Nothing
-- foo = segmentAbove (Point2 10 0) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
-- foo = segmentAbove (Point2 10 2) test1 `shouldBe` Just (LineSegment (Closed (Point2 1 4 :+ ())) (Closed (Point2 12 4 :+ ())) :+ 2)
-- foo = segmentAbove (Point2 10 1) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
