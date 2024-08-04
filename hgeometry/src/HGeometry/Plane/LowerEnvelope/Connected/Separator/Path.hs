{-# LANGUAGE  UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Path
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.Path
  ( -- NodeSplit(..)

  -- , Path(..)
  -- , collectPath
  -- , trimap, trifoldMap

  -- , findNode
  -- , findNode'
  -- , findNodeAlongPath
  -- , Side(..)
  ) where

import           Control.Applicative
import           Control.Lens ((<&>))
import           Data.Bifoldable
import           Data.Bifunctor
import qualified Data.Foldable as F
import           Data.Kind (Type)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid (First(..))
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Graph
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight
import           HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
import           HGeometry.Vector

import           Debug.Trace

--------------------------------------------------------------------------------
-- * Paths
