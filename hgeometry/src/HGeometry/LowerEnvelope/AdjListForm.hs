{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LowerEnvelope.AdjListForm
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.LowerEnvelope.AdjListForm
  ( LowerEnvelope(..)
  , LowerEnvelope'(LowerEnvelope)
  , theUnboundedVertex, boundedVertices

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

import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LowerEnvelope.Connected
import qualified HGeometry.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

-- | The lower enevelope of planes in R^3
data LowerEnvelope plane = ParallelStrips () -- TODO stuff here
                         -- make sure this also works in case there is only one plane ...
                         | ConnectedEnvelope !(LowerEnvelope' plane)

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq   (LowerEnvelope plane)

type instance NumType   (LowerEnvelope plane) = NumType plane
type instance Dimension (LowerEnvelope plane) = 3


--------------------------------------------------------------------------------

-- | Given a Lower envelope in vertex form, construct the AdjacencyList representation out
-- of it.
--
-- \(O(n\log n)\)
fromVertexForm      :: forall plane r. (Plane_ plane r, Ord plane, Ord r, Fractional r
                                       , Show plane, Show r
                                       )
                    => VertexForm.VertexForm plane -> LowerEnvelope plane
fromVertexForm lEnv
  | VertexForm.hasVertices lEnv = ConnectedEnvelope $ fromVertexForm' lEnv
  | otherwise                   = ParallelStrips () -- TODO
