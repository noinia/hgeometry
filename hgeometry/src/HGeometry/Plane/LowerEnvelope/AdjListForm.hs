{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.AdjListForm
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.AdjListForm
  ( LowerEnvelope(..)
  , LowerEnvelope'(LowerEnvelope)
  , ParallelPlane
  , theUnboundedVertex, boundedVertices
  , IntersectionLine(..)

  , singleton
  , fromVertexForm

  , BoundedVertexF(Vertex)
  , location, definers, location2

  , UnboundedVertex(UnboundedVertex)
  , unboundedVertexId
  , HasUnboundedEdges(..)

  , EdgeGeometry
  , projectedEdgeGeometries, projectedEdgeGeometry
  ) where


--------------------------------------------------------------------------------

import           Control.Lens
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           HGeometry.Algorithms.DivideAndConquer
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line.LineEQ
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Plane.LowerEnvelope.VertexForm (IntersectionLine(..), intersectionLine)
import qualified HGeometry.Plane.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()
import Data.Ord (comparing)
import Data.Function (on)

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

-- | The lower enevelope of planes in R^3. (Or rather, its minimization diagram)
data LowerEnvelope plane =
    ParallelStrips    !(Set.Set (ParallelPlane plane))
  | ConnectedEnvelope !(LowerEnvelope' plane)

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (LowerEnvelope plane)

type instance NumType   (LowerEnvelope plane) = NumType plane
type instance Dimension (LowerEnvelope plane) = 3

-- | Just a newtype around plane, to be used to model parallel strips in the Lower envelope.
newtype ParallelPlane plane =
  ParallelPlane plane deriving (Show,Eq)

instance Wrapped (ParallelPlane plane) where
  type Unwrapped (ParallelPlane plane) = plane
  _Wrapped' = coerced

-- instance Rewrapped (ParallelPlane plane) plane

--------------------------------------------------------------------------------

-- | Given a Lower envelope in vertex form, construct the AdjacencyList representation out
-- of it.
--
-- \(O(n\log n)\)
fromVertexForm          :: forall f plane r. ( Plane_ plane r, Ord plane, Ord r, Fractional r
                                             , Show plane, Show r
                                             , Foldable1 f
                                             )
                        => f plane -> VertexForm.VertexForm plane -> LowerEnvelope plane
fromVertexForm hs lEnv
    | VertexForm.hasVertices lEnv = ConnectedEnvelope $ fromVertexForm' lEnv
    | otherwise                   = ParallelStrips $ Set.fromDistinctAscList (F.toList strips)
  where
    strips :: NonEmpty (ParallelPlane plane)
    strips = coerce
           $ divideAndConquer1With (mergeAndDiscardBy cmpPlanes) NonEmpty.singleton hs

-- withBisectors :: Plane_ plane r
--               => f plane -> Alternating.Alternating Set.Set plane (IntersectionLine r)
-- withBisectors = undefined

-- | Orders the planes from "left to right" (or from top to bottom) for nor non-vertical ones
cmpPlanes      :: forall plane r. (Plane_ plane r, Ord r, Fractional r)
               => plane -> plane -> XOrdering
cmpPlanes h h' = case intersectionLine h h' of
    Nothing | evalAt q h <= evalAt q h' -> Dominates
            | otherwise                 -> Dominated
              where q = origin :: Point 2 r
    Just (Vertical x)                   -> cmp' $ Point2 (x-1) 0
    Just (NonVertical (LineEQ _ b))     -> cmp' $ Point2 0     (b+1)
  where
    cmp' q = case evalAt q h `compare` evalAt q h' of
               LT -> XLT
               EQ -> XEQ
               GT -> XGT

-- | An ordering type that also allows us to discard dominated entries
data XOrdering = XLT | XEQ | XGT | Dominates | Dominated deriving (Show,Eq,Ord)

mergeAndDiscardBy     :: (a -> a -> XOrdering) -> NonEmpty a -> NonEmpty a -> NonEmpty a
mergeAndDiscardBy cmp = go
  where
    go as@(a:|as') bs@(b:|bs') = case a `cmp` b of
      XLT       -> a NonEmpty.<| goL as' bs
      XEQ       -> a :|          goB as' bs'
      XGT       -> b NonEmpty.<| goR as  bs'
      Dominates -> goR as  bs'
      Dominated -> goL as' bs

    goR as bs = case NonEmpty.nonEmpty bs of
                  Nothing  -> as
                  Just bs' -> go as bs'
    goL as bs = case NonEmpty.nonEmpty as of
                  Nothing  -> bs
                  Just as' -> go as' bs
    goB as bs = case NonEmpty.nonEmpty as of
                  Nothing  -> bs
                  Just as' -> F.toList $ goR as' bs


--- >>> testOrdering (1,1) (1,4)
-- Dominates
testOrdering               :: (Int,Int) -> (Int,Int) -> XOrdering
testOrdering (a,b) (a',b') = case a `compare` a' of
                               LT -> XLT
                               GT -> XGT
                               EQ -> case b `compare` b' of
                                       LT -> Dominates
                                       GT -> Dominated
                                       EQ -> XEQ

answer       :: Ord a => NonEmpty (a,a) -> NonEmpty (a,a) -> NonEmpty (a,a)
answer lA lB =
  fmap NonEmpty.head . NonEmpty.groupBy1 ((==) `on` fst) . NonEmpty.sort $ lA <> lB

-- >>> test
-- True

-- >>> mergeAndDiscardBy testOrdering listA listB
-- (1,1) :| [(2,2),(3,1),(4,2),(5,2),(7,1)]
--

-- >>> answer listA listB
-- (1,1) :| [(2,2),(3,1),(4,2),(5,2),(7,1)]
test :: Bool
test = testMerge listA listB

testMerge      :: NonEmpty (Int,Int) -> NonEmpty (Int,Int) -> Bool
testMerge lA lB = mergeAndDiscardBy testOrdering lA lB
                  `shouldBe`
                  answer lA lB


shouldBe :: NonEmpty (Int, Int) -> NonEmpty (Int, Int) -> Bool
shouldBe = (==)


listA = NonEmpty.fromList $ [ (1,4)
                            , (2,3)
                            , (2,5)
                            , (2,7)
                            , (4,2)
                            , (5,2)
                            , (7,1)
                            ]
listB = NonEmpty.fromList $ [ (1,1)
                            , (2,2)
                            , (3,1)
                            , (4,2)
                            , (5,10)
                            ]
