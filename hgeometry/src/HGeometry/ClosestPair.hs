--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ClosestPair
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(O(n\log n)\) time algorithm to compute the
-- closest pair among a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module HGeometry.ClosestPair( closestPair ) where

import HGeometry.ClosestPair.DivideAndConquer
