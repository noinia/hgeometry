{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional vectors.
--
--------------------------------------------------------------------------------
module HGeometry.Vector(
    module HGeometry.Vector.Class
  -- , module HGeometry.Vector.Optimal
  , generate
  , isScalarMultipleOf
  , scalarMultiple
  , sameDirection

  -- , Optimize(..), Choose
  ) where

import HGeometry.Vector.Class
import HGeometry.Vector.Impl
