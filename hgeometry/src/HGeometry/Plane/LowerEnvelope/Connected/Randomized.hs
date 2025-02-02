--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected.Randomized
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- expected O(n polylog n) divide and conquer implementation
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Randomized
  ( computeVertexForm
  ) where

import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Permutation.Shuffle
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.Point
import           Prelude hiding (filter)
import           System.Random
import           Witherable

--------------------------------------------------------------------------------

-- n_0 :: Int
-- n_0 = 2

--------------------------------------------------------------------------------

computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set
                            , RandomGen gen
                            , Show plane, Show r
                            )
                         => gen -> set plane -> VertexForm r plane
computeVertexForm gen = computeVertexForm' . shuffle gen


-- | pre: imput is already a random permutation
computeVertexForm'  :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                       , Show plane, Show r
                       )
                    => V.Vector plane -> VertexForm r plane
computeVertexForm' planes = undefined
  -- | n <= n_0  = BruteForce.computeVertexForm planes
  -- -- no need to check this; I think; in this case the onflict lists will just be empty
  -- | otherwise = undefined
  where
    n    = length planes
    r    = sqrt . sqrt @Double . fromIntegral $ n
    rNet = V.take (round $ r * logBase 2 r) planes


    -- res = bruteForceLowerEnvelope rNet


-- | Computes conflict list
withConflictLists        :: (Witherable set, Plane_ plane r, Ord r, Num r)
                         => set plane
                         -> VertexForm r plane
                         -> Map (Point 3 r) (Definers plane, set plane)
withConflictLists planes = Map.mapWithKey (\v defs -> (defs, filter (below v) planes))
  where
    below v h = verticalSideTest v h == LT -- TODO: not sure if this should be LT or 'not GT'
-- TODO: dummy implementation for now
