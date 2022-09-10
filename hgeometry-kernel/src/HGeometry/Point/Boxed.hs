{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Boxed
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module HGeometry.Point.Boxed
  ( Point(..)
  , Arity
  ) where

import HGeometry.Point.EuclideanDistance
import HGeometry.Point.Internal

--------------------------------------------------------------------------------
-- $setup
-- >>> import HGeometry.Vector
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = Vector3 1 2 3
--     myPoint = Point myVector
-- :}

instance Num r => HasSquaredEuclideanDistance (Point d r) where
  pointClosestTo _ = id
