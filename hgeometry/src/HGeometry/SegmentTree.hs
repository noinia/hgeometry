{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.SegmentTree
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Segment Tree implementation
--
--------------------------------------------------------------------------------
module HGeometry.SegmentTree
  ( SegmentTree
  , buildSegmentTree
  , buildSkeleton

  , stab
  , query

  , insert

  -- , HasCanonicalSubSet(..)
  , ascEndPoints
  -- , elementaryIntervals
  -- , ElementaryInterval(..)
  ) where

import HGeometry.SegmentTree.Base
