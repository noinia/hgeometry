--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.VerticalRayShooting.PersistentSweep
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.VerticalRayShooting.PersistentSweep
  ( VerticalRayShootingStructure
  , StatusStructure
  -- , leftMost, sweepStruct

  -- * Building the Data Structure
  , verticalRayShootingStructure
  , verticalRayShootingStructure'
  -- * Querying the Data Structure
  , segmentAbove, segmentAboveOrOn
  , findSlab
  , lookupAbove, lookupAboveOrOn, searchInSlab
  ) where

import           Control.DeepSeq
import           GHC.Generics (Generic)
import           Control.Lens hiding (contains, below)
import           Data.Foldable (toList)
import           Data.Functor.Contravariant (phantom)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import qualified Data.Set as SS -- status struct
import qualified Data.Vector as V
import           HGeometry.Algorithms.BinarySearch (binarySearchFirstIn)
import           HGeometry.Ext
import           HGeometry.Line.PointAndVector
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Properties
import qualified HGeometry.Set.Util as SS

--------------------------------------------------------------------------------

-- | The vertical ray shooting data structure
type VerticalRayShootingStructure lineSegment =
  VerticalRayShootingStructure' (NumType lineSegment) lineSegment

-- | The implementatyion of the vertical ray shooting data structure
data VerticalRayShootingStructure' r lineSegment =
    VerticalRayShootingStructure { _leftMost    :: !r
                                 -- ^ x-coordinate of the leftmost vertex/endpoint
                                 , _sweepStruct :: V.Vector (r :+ StatusStructure lineSegment)
                                   -- ^ entry (r :+ s) means that "just" left of "r" the
                                   -- status structure is 's', i.e up to 'r'
                                 } deriving (Show,Eq,Generic)

-- TODO this is very similar to the 'Alternating' sequence structure; so see if we can reuse code

instance (NFData r, NFData lineSegment) => NFData (VerticalRayShootingStructure' r lineSegment)

-- | The status structure
type StatusStructure lineSegment = SS.Set lineSegment

-- | Getter to access the leftmost coordinate.
leftMost :: Getter (VerticalRayShootingStructure' r lineSegment) r
leftMost f (VerticalRayShootingStructure x _) = phantom (f x)
{-# INLINE leftMost #-}

-- | Getter to access the sweep structure
sweepStruct :: Getter (VerticalRayShootingStructure' r lineSegment)
                      (V.Vector (r :+ StatusStructure lineSegment))
sweepStruct f (VerticalRayShootingStructure _ ss) = phantom (f ss)
{-# INLINE sweepStruct #-}

-- more or less copied the above two implementations from the TH generated ones

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
verticalRayShootingStructure    :: ( LineSegment_ lineSegment point
                                   , Point_ point 2 r
                                   , Ord r, Fractional r, Foldable1 nonEmpty)
                                => nonEmpty lineSegment
                                -> VerticalRayShootingStructure lineSegment
verticalRayShootingStructure ss =
    VerticalRayShootingStructure (eventX e) (sweep' events)
  where
    events@(e :| _) = fmap combine
                    . NonEmpty.groupAllWith1 eventX
                    . foldMap1 (toEvents . orientLR)
                    $ ss
    sweep' = V.fromList . toList . sweep

    toEvents seg = NonEmpty.fromList [ (seg^.start.xCoord) :+ Insert seg :| []
                                     , (seg^.end.xCoord)   :+ Delete seg :| []
                                     ]


-- | Given a set of \(n\) interiorly pairwise disjoint *closed*
-- segments, try to construct a vertical ray shooting data
-- structure. This function will remove all vertical line segments. If
-- there are no non-vertical segments, we return a Nothing.
--
-- The endpoints of the segments are allowed to coincide.
--
-- running time: \(O(n\log n)\).
-- space: \(O(n\log n)\).
verticalRayShootingStructure' :: ( LineSegment_ lineSegment point
                                , Point_ point 2 r
                                , Ord r, Fractional r, Foldable1 nonEmpty)
                              => nonEmpty lineSegment
                              -> Maybe (VerticalRayShootingStructure lineSegment)
verticalRayShootingStructure' =
    fmap verticalRayShootingStructure . witherNE isNonVertical . toNonEmpty
  where
    witherNE p = NonEmpty.nonEmpty . NonEmpty.filter p
    isNonVertical seg = seg^.start.xCoord /= seg^.end.xCoord


-- | Given a bunch of events happening at the same time, merge them into a single event
-- where we apply all actions.
combine                    :: NonEmpty (Event r lineSegment) -> Event r lineSegment
combine es@((x :+ _) :| _) = x :+ foldMap1 eventActions es

----------------------------------------

data Action a = Insert a | Delete a  deriving (Show,Eq)

{- HLINT ignore "Avoid lambda using `infix`" -}
interpret :: Action a -> (a -> a -> Ordering) -> SS.Set a -> SS.Set a
interpret = \case
  Insert s -> \cmp -> SS.insertBy    cmp s
  Delete s -> \cmp -> SS.deleteAllBy cmp s


-- | An event; i.e. an x coordinate together with a bunch of actions.
type Event r lineSegment = r :+ NonEmpty (Action lineSegment)

-- | The x-coordinate at which an event happens
eventX :: Event r lineSegment -> r
eventX = view core

-- | The actions at a particular event
eventActions :: Event r lineSegment -> NonEmpty (Action lineSegment)
eventActions = view extra

----------------------------------------

-- | Runs the sweep building the data structure from left to right.
sweep    :: ( LineSegment_ lineSegment point, Point_ point 2 r
            , Ord r, Fractional r
            )
         => NonEmpty (Event r lineSegment) -> NonEmpty (r :+ StatusStructure lineSegment)
sweep es = NonEmpty.fromList
         . snd . List.mapAccumL h SS.empty
         $ zip (toList es) (NonEmpty.tail es)
  where
    h ss evts = let x :+ ss' = handle ss evts in (ss',x :+ ss')

-- | Given the current status structure (for left of the next event
-- 'l'), and the next two events (l,r); essentially defining the slab
-- between l and r, we construct the status structure for in the slab (l,r).
-- returns the right boundary and this status structure.
handle                :: (Ord r, Fractional r, LineSegment_ lineSegment point, Point_ point 2 r)
                      => StatusStructure lineSegment
                      -> (Event r lineSegment, Event r lineSegment)
                      -> r :+ StatusStructure lineSegment
handle ss ( l :+ acts
          , r :+ _)   = let mid               = (l+r)/2
                            runActionAt x act = interpret act (ordAtX x)
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
segmentAbove      :: ( LineSegment_ lineSegment point, Point_ point 2 r
                     , Point_ queryPoint 2 r
                     , Ord r, Num r, HasSupportingLine lineSegment
                     ) => queryPoint -> VerticalRayShootingStructure lineSegment
                  -> Maybe lineSegment
segmentAbove q ds = findSlab q ds >>= lookupAbove q

-- | Find the segment vertically query point q, if it exists.
--
-- \(O(\log n)\)
segmentAboveOrOn       :: ( LineSegment_ lineSegment point, Point_ point 2 r
                          , Point_ queryPoint 2 r
                          , Ord r, Num r, HasSupportingLine lineSegment
                          ) => queryPoint -> VerticalRayShootingStructure lineSegment
                       -> Maybe lineSegment
segmentAboveOrOn q ds = findSlab q ds >>= lookupAboveOrOn q



-- | Given a query point, find the (data structure of the) slab containing the query point
--
-- \(O(\log n)\)
findSlab  :: ( LineSegment_ lineSegment point, Point_ point 2 r
             , Point_ queryPoint 2 r
             , Ord r, Num r, HasSupportingLine lineSegment
             )
          => queryPoint -> VerticalRayShootingStructure lineSegment
          -> Maybe (StatusStructure lineSegment)
findSlab q ds | q^.xCoord < ds^.leftMost = Nothing
              | otherwise                = view extra
                                        <$> binarySearchFirstIn (q `leftOf `) (ds^.sweepStruct)
  where
    q' `leftOf` (r :+ _) = q'^.xCoord <= r

--------------------------------------------------------------------------------
-- * Querying in a single slab

-- | Finds the segment containing or above the query point 'q'
--
-- \(O(\log n)\)
lookupAboveOrOn   :: ( LineSegment_ lineSegment point, Point_ point 2 r
                     , Point_ queryPoint 2 r
                     , Ord r, Num r, HasSupportingLine lineSegment
                     )
                  => queryPoint -> StatusStructure lineSegment -> Maybe lineSegment
lookupAboveOrOn q = searchInSlab (not . (q `liesAbove`))

-- | Finds the first segment strictly above q
--
-- \(O(\log n)\)
lookupAbove   :: ( LineSegment_ lineSegment point, Point_ point 2 r
                 , Point_ queryPoint 2 r
                 , Ord r, Num r, HasSupportingLine lineSegment
                 )
              => queryPoint -> StatusStructure lineSegment -> Maybe lineSegment
lookupAbove q = searchInSlab (q `liesBelow`)

-- | generic searching function
searchInSlab   :: (LineSegment_ lineSegment point, Point_ point 2 r
                  , HasSupportingLine lineSegment, Num r)
               => (LinePV 2 r -> Bool)
               -> StatusStructure lineSegment -> Maybe lineSegment
searchInSlab p = binarySearchFirstIn (p . supportingLine)


----------------------------------------------------------------------------------
