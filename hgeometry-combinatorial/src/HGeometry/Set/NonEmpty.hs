--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Set.NonEmpty
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Utilities for working with NonEmpty sets
--
--------------------------------------------------------------------------------
module HGeometry.Set.NonEmpty
  ( extractMinimaBy

  ) where

import HGeometry.NonEmpty.Util qualified as NonEmpty
import Data.Set.NonEmpty qualified as NESet
import HGeometry.Ext
import Data.Set qualified as Set
import Data.Bifunctor

--------------------------------------------------------------------------------

-- | Extract the smallest element according to the given ordering form the set.
extractMinimaBy     :: (a -> a -> Ordering) -> NESet.NESet a -> NESet.NESet a :+ Set.Set a
extractMinimaBy cmp = bimap NESet.fromDistinctAscList Set.fromDistinctAscList
                    . NonEmpty.extractMinimaBy cmp . NESet.toAscList
