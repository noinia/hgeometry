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

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Foldable1.WithIndex
import           Data.Function (on)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList)
import           Data.Ord (comparing)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Traversable (mapAccumL)
import qualified Data.Vector as V
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           Debug.Trace
import           HGeometry.Box
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.LowerEnvelope.Connected
import           HGeometry.LowerEnvelope.Type
import           HGeometry.LowerEnvelope.VertexForm (IntersectionLine(..),intersectionLine)
import qualified HGeometry.LowerEnvelope.VertexForm as VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import qualified HGeometry.Vector as Vec
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph
import           Witherable

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope


data LowerEnvelope plane = ParallelStrips () -- TODO stuff here
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
