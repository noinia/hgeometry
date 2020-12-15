{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SmallestEnclosingBall
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Types to represent the smallest enclosing disk of a set of points in
-- \(\mathbb{R}^2\)
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.SmallestEnclosingBall
  ( DiskResult(..)
  , enclosingDisk
  , definingPoints
  , TwoOrThree(..)
  , twoOrThreeFromList
  ) where

import           Algorithms.Geometry.SmallestEnclosingBall.Types
