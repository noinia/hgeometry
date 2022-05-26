{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.SPM.EdgeSPM
  ( WithDistance(WithDistance), distanceToSource, predecessorToSource

  , Generator(Generator), root, initialDist, unfoldedRoot
  , FrontierPoint(..)

  , SPMInterval(SPMInterval), generator, subInterval, frontierPoint


  , EdgeSubdivision
  , EdgeSPM
  ) where

import qualified Algorithms.Geometry.SPM.PSQueueUtil as PSQueue
import           Control.Lens
import           Data.Coerce
import           Data.DynamicOrd
import           Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict as EnumMap
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe (maybeToList, fromMaybe)
import qualified Data.PSQueue as PSQueue
import           Data.Radical
import qualified Data.Set as Set
import           Data.Set.Util (genericSplitBy)
import           Data.UnBounded
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Geometry.Interval
import           Geometry.Line
import           Geometry.LineSegment hiding (endPoints)
import           Geometry.PlanarSubdivision
import           Geometry.Point
import           Geometry.Point (euclideanDist)
import           Witherable

--------------------------------------------------------------------------------

-- | Distance and predecessor towards the source
data WithDistance s r = WithDistance { _distanceToSource    :: !r
                                       -- ^ distance to the source
                                     , _predecessorToSource :: !(Maybe (VertexId' s))
                                       -- ^ Nothing if this is the source itself.
                                     }
                      deriving (Show)

makeLenses ''WithDistance

-- | Compare by distance
instance Eq r => Eq (WithDistance s r) where
  (WithDistance a _) == (WithDistance b _) = a == b
instance Ord r => Ord (WithDistance s r) where
  (WithDistance a _) `compare` (WithDistance b _) = a `compare` b

--------------------------------------------------------------------------------

-- | A generator gives rise to a candidate interval on an edge.
data Generator s r =
  Generator { _root         :: !(VertexId' s)
              -- ^ the root/apex of the points in the interval
            , _initialDist  :: !(WithDistance s r)
              -- ^ distance to the root
            , _unfoldedRoot :: !(Point 2 r)
              -- ^ point corresponding to the root in the same
              -- coordinate system as the face opposite to the edge
              -- this generator corresponds to.
              --
              -- note that in the plane the unfolded root is just the
              -- same as the original location of the root.
            } deriving (Show)

makeLenses ''Generator

-- | First point in an interval that is discovered
data FrontierPoint s r = VertexEvent   (VertexId' s) (Point 2 r)
                       | InteriorEvent (Point 2 r)
                       deriving (Show,Eq)

-- | An SPM Interval
data SPMInterval s r =
  SPMInterval { _generator     :: !(Generator s r)
              -- ^ the generator for the interval
              , _subInterval   :: !(Interval (Point 2 r) r)
              -- ^ the actual interval/range on the edge. The points
              -- are the actual points in R^2, whereas the r values
              -- are values in the range [0,1]
              , _frontierPoint :: !(FrontierPoint s r)
              }
  deriving (Show)

makeLenses ''SPMInterval


instance Ord r => Eq (SPMInterval s r) where
  int1 == int2 = int1 `compare` int2 == EQ
-- | order by left endpoint, note that this ordering makes sense only
-- if both intervals live on the same edge.
instance Ord r => Ord (SPMInterval s r) where
  int1 `compare` int2 = ((int1^.start.core) `compare` (int2^.start.core))
                        <>
                        ((int1^.end.core) `compare` (int2^.end.core))
    -- on equal startpoints we fall back to comparing endpoints as
    -- well, so that we have a proper equality test.

instance HasStart (SPMInterval s r) where
  type StartCore (SPMInterval s r) = r
  type StartExtra (SPMInterval s r) = Point 2 r
  start = subInterval.start

instance HasEnd (SPMInterval s r) where
  type EndCore (SPMInterval s r) = r
  type EndExtra (SPMInterval s r) = Point 2 r
  end = subInterval.end

--------------------------------------------------------------------------------

-- | A halfedge is subdivided into intervals, they are stored in order
-- along the halfedge.
newtype EdgeSubdivision s r = EdgeSubdivision (PSQueue.PSQ (SPMInterval s r)
                                                           (WithDistance s r))
                            deriving (Show)


-- | Initializes an subdivision with no reachable intervals
unreachableEdge :: Ord r => EdgeSubdivision s r
unreachableEdge = EdgeSubdivision PSQueue.empty



orderPoint       :: (Ord r, Num r) => Point 2 r -> SPMInterval s r -> Ordering
orderPoint c int = case c `onSide` line of
                     LeftSide -> LT
                     OnLine   -> if isOpen s then LT else EQ
                     RightSide -> case c `onSide` (Line (int^.end.extra) v) of
                       LeftSide  -> EQ -- we are in the interval
                       OnLine    -> if isOpen t then GT else EQ
                       RightSide -> GT
  where
    line@(Line _ v) = perpendicularTo $ lineThrough (int^.start.extra) (int^.end.extra)
    Interval s t = int^.subInterval


-- | computes the intervals strictly left of the point, the interval
-- containing the point (if ite xists), and the intervals right of the
-- point.
locatePoint :: (Ord r, Num r)
            => Point 2 r -- ^ point to locate
            -> EdgeSubdivision s r -- ^ the edge subdivision
            -> ( EdgeSubdivision s r
               , Maybe (SPMInterval s r, WithDistance s r)
               , EdgeSubdivision s r
               )
locatePoint c (EdgeSubdivision psq) = case PSQueue.splitBy (orderPoint c) psq of
    (l,m,r) -> (EdgeSubdivision l, undefined ,EdgeSubdivision r)
      -- just view the min of the middle


-- | Inserts a new edge into the subdivision
insertInterval            :: (Ord r, Radical r)
                          => SPMInterval s r -> EdgeSubdivision s r -> EdgeSubdivision s r
insertInterval int subdiv = case locatePoint c subdiv of
    (ls,m,rs) -> let (EdgeSubdivision ls',l) = trimR ls m int
                     (EdgeSubdivision rs',r) = trimL m rs int
                     int'    = int&subInterval.start %~ f l
                                  &subInterval.end   %~ f r
                 in EdgeSubdivision $ ls' `PSQueue.join` (PSQueue.insert int' dist rs')
  where
    dist = WithDistance (euclideanDist c $ int^.generator.unfoldedRoot)
                        (Just $ int^.generator.root)

    c = case (int^.frontierPoint) of
          VertexEvent i c' -> c'
          InteriorEvent c' -> c'

    f (x :+ p) z = z&core .~ x
                    &extra .~ p


trimR :: EdgeSubdivision s r
      -> Maybe (SPMInterval s r, WithDistance s r)
      -> SPMInterval s r -> (EdgeSubdivision s r, r :+ Point 2 r)
trimR (EdgeSubdivision subdiv) mb int = case mb of
    Nothing                                -> rest
    Just (intB,_) | intB `dominatedBy` int -> rest
                  | otherwise              ->  undefined
  where
    rest = trimFirst $ PSQueue.dropWhile (dominatedBy int) subdiv

intA `dominatedBy` intB = undefined

trimFirst = undefined

trimL = undefined

--------------------------------------------------------------------------------


data Edge s r = Edge (Dart s) (EdgeSubdivision s r)
              deriving (Show)

-- | Equality and Ordering is based only on the dart
instance Eq (Edge s r) where
  (Edge d _) == (Edge d' _) = d == d'
-- | Equality and Ordering is based only on the dart
instance Ord (Edge s r) where
  (Edge d _) `compare` (Edge d' _) = d `compare` d'

newtype EdgeSPM s r = EdgeSPM (PSQueue.PSQ (Edge s r)
                                           (WithDistance s r))
                    deriving (Show)

-- createEdgeSPM :: (Foldable f, Ord r) => f (Dart s) -> EdgeSPM s r
-- createEdgeSPM = EdgeSPM . PSQueue.fromDistinctAscList . map f . F.toList
--   where
--     f d = (Edge d unreachableEdge) PSQueue.:->
