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
    VectorLike_(..)
  , vectorFromList
  , Additive_(..)

  , Vector
  , generate
  , component, xComponent, yComponent, zComponent, wComponent

  , zero, (^+^), (^-^), lerp, negated, (*^), (^*), (^/), liftI2, sumV, basis, unit
  , foldMapZip


  , dot, quadrance, qd, norm, signorm

  , sameDirection
  , scalarMultiple
  , isScalarMultipleOf
  ) where

import HGeometry.Vector.Class
import HGeometry.Vector.Impl
import Vector
