--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.BruteForce
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The O(n^4) time algorithm.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.BruteForce
  ( bruteForceLowerEnvelope
  , bruteForceLowerEnvelopeIn
  , connectedLowerEnvelopeWith
  , computeVertexForm

  , belowAll
  ) where

import           Control.Lens
import           Control.Monad(void)
import           Data.Foldable1 as F1
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (pattern IsEmpty, pattern IsNonEmpty)
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.Plane.LowerEnvelope.Clipped.Type
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Region
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.Point
import           HGeometry.Triangle

--------------------------------------------------------------------------------
-- * Computing the (minimization diagram of) the Lower envelope

-- | Computes the lower envelope in O(n^4) time using a brute force approach
--
-- The naive O(n^4) time algorithm.
bruteForceLowerEnvelope :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                           , Foldable set
                           , Show r, Show plane
                           )
                        => set plane
                        -> Maybe (MinimizationDiagram r (MDVertex r plane ()) plane)
bruteForceLowerEnvelope = connectedLowerEnvelopeWith computeVertexForm

-- | Given an algorithm to compute the vertices of the lower envelope, computes
-- the actual lower envelope. Returns a Nothing if there are no vertices
--
connectedLowerEnvelopeWith :: (Plane_ plane r, Ord r, Fractional r, Foldable set, Ord plane
                              , Show plane, Show r
                              )
                           => (set plane -> VertexForm Map r plane)
                           -> set plane
                           -> Maybe (MinimizationDiagram r (MDVertex r plane ()) plane)
connectedLowerEnvelopeWith computeVertexForm' planes = case computeVertexForm' planes of
  IsNonEmpty vertices' -> Just . mapVertices void $ fromVertexForm vertices'
                          -- the mapVertices replaces the data value in the MDVertex data by ()
  IsEmpty              -> Nothing


--------------------------------------------------------------------------------
-- * Computing the (minimization diagram of) the Lower envelope clipped to a triangle

-- | Use a navive O(n^4) time algorithm to compute the lower envelope inside a given
-- triangle.
bruteForceLowerEnvelopeIn            :: forall plane r corner set.
                                        ( Plane_ plane r, Ord plane, Ord r, Fractional r
                                        , Point_ corner 2 r
                                        , Foldable1 set

                                        , Show r, Show corner, Show plane -- TODO: remove these
                                        )
                                     => Triangle corner
                                     -> set plane
                                     -> ClippedMinimizationDiagram plane
bruteForceLowerEnvelopeIn tri planes = review _ClippedMinimizationDiagramMap
                                     . fromMinimizationDiagramIn tri planes
                                     $ bruteForceLowerEnvelope planes

--------------------------------------------------------------------------------
-- * Computing the vertices of the lower envelope

-- | Computes the vertices of the lower envelope
--
-- O(n^4) time.
computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set
                            , Show plane, Show r
                            )
                         => set plane -> VertexForm Map r plane
computeVertexForm planes = unionsWithKey mergeDefiners
                         . map (asVertex planes) $ uniqueTriplets planes

asVertex             :: (Plane_ plane r, Foldable f, Ord plane, Ord r, Fractional r)
                     => f plane -> Three plane -> Map (Point 3 r) (Definers plane)
asVertex planes defs = case definers defs of
  Just (v,defs')  | v `belowAll` planes -> Map.singleton v defs'
  _                                     -> Map.empty

-- | test if v lies below (or on) all the given planes
belowAll   :: (Plane_ plane r, Ord r, Num r, Foldable f) => Point 3 r -> f plane -> Bool
belowAll v = all (\h -> verticalSideTest v h /= GT)
{-# INLINE belowAll #-}


--------------------------------------------------------------------------------
