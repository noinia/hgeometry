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
  , computeVertexForm
  ) where

import           Data.Foldable1
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.Point

--------------------------------------------------------------------------------
-- * The naive O(n^4) time algorithm.

-- | Computes the lower envelope in O(n^4) time.
--
-- pre: there are vertices
--
bruteForceLowerEnvelope :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                           , Foldable set
                           , Show r, Show plane
                           )
                        => set plane
                        -> MinimizationDiagram r (Point 2 r :+ Definers plane) plane
bruteForceLowerEnvelope = fromVertexForm . computeVertexForm

-- | Computes the vertices of the lower envelope
--
-- pre: there are vertices
--
-- O(n^4) time.
computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set
                            , Show plane, Show r
                            )
                         => set plane -> VertexForm r plane
computeVertexForm planes = NEMap.unsafeFromMap
                         . unionsWithKey mergeDefiners
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
