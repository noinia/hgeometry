{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Vector
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional vectors.
--
--------------------------------------------------------------------------------
module HGeometry.Vector(
    module HGeometry.Vector.Class
  , Vector(Vector,Vector1,Vector2,Vector3,Vector4)
  , Arity
  -- , quadrance
  -- , dot, norm, signorm
  -- , isScalarMultipleOf
  -- , scalarMultiple, sameDirection
  --                   -- reexports
  -- , FV.replicate
  ) where

import HGeometry.Vector.Internal
import HGeometry.Vector.Class
