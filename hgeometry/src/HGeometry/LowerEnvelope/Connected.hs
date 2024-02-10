{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LowerEnvelope.Connected
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.LowerEnvelope.Connected
  ( LowerEnvelope'(LowerEnvelope)
  , theUnboundedVertex, boundedVertices

  , singleton
  , fromVertexForm'

  , BoundedVertexF(Vertex)
  , location, definers, location2

  , UnboundedVertex(UnboundedVertex)
  , unboundedVertexId
  , HasUnboundedEdges(..)

  , EdgeGeometry
  , projectedEdgeGeometries, projectedEdgeGeometry
  ) where

import           HGeometry.LowerEnvelope.Type
import           HGeometry.LowerEnvelope.Connected.Type
import           HGeometry.LowerEnvelope.Connected.FromVertexForm
import           HGeometry.Vector.NonEmpty.Util ()


--------------------------------------------------------------------------------
