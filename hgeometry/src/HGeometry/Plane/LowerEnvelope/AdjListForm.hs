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
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
-- import           HGeometry.Algorithms.DivideAndConquer
-- import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
-- import           HGeometry.Line.LineEQ
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Plane.LowerEnvelope.VertexForm (IntersectionLine(..))
import qualified HGeometry.Plane.LowerEnvelope.VertexForm as VertexForm
-- import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()

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
fromVertexForm _hs lEnv
    | VertexForm.hasVertices lEnv = ConnectedEnvelope $ fromVertexForm' lEnv
    | otherwise                   = ParallelStrips $ Set.fromDistinctAscList (F.toList strips)
  where
    strips :: NonEmpty (ParallelPlane plane)
    strips = error "TODO"
      -- coerce
      --      $ divideAndConquer1With (mergeAndDiscardBy cmpPlanes) NonEmpty.singleton hs

-- withBisectors :: Plane_ plane r
--               => f plane -> Alternating.Alternating Set.Set plane (IntersectionLine r)
-- withBisectors = undefined
