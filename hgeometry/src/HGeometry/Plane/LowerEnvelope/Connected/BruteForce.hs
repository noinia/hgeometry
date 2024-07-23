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

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Semigroup (First(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- * The naive O(n^4) time algorithm.

-- | Computes the lower envelope in O(n^4) time.
bruteForceLowerEnvelope :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                           , Foldable set
                           -- , Show r, Show plane
                           ) => set plane -> MinimizationDiagram r plane
bruteForceLowerEnvelope = fromVertexForm . computeVertexForm

-- | Computes the vertices of the lower envelope
--
-- O(n^4) time.
computeVertexForm        :: (Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set)
                         => set plane -> VertexForm r plane
computeVertexForm planes = unionsWithKey mergeDefiners
                         . map (asVertex planes) $ uniqueTriplets planes

asVertex             :: (Plane_ plane r, Foldable f, Ord plane, Ord r, Fractional r)
                     => f plane -> Three plane -> Map (Point 3 r) (Definers plane)
asVertex planes defs = case definers defs of
  Just (v,defs')  | v `belowAll` planes -> Map.singleton v defs'
  _                                     -> Map.empty

-- | test if v lies below (or on) all the given planes
belowAll   :: (Plane_ plane r, Ord r, Num r, Foldable f) => Point 3 r -> f plane -> Bool
belowAll v = all (\h -> onSideTest v h /= GT)
{-# INLINE belowAll #-}


--------------------------------------------------------------------------------
