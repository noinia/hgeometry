{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Point.Boxed
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Geometry.Point.Boxed
  ( Point(..)
  , vector
  , fromGenericPoint
  -- , pattern Point1
  -- , pattern Point2
  -- , pattern Point3
  -- , pattern Point4
  , PointFunctor(..)
  ) where

import Geometry.Point.EuclideanDistance
import Geometry.Point.Internal

--------------------------------------------------------------------------------
-- $setup
-- >>> import Geometry.Vector
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = Vector3 1 2 3
--     myPoint = Point myVector
-- :}

instance Num r => HasSquaredEuclideanDistance (Point d r) where
  pointClosestTo _ = id
