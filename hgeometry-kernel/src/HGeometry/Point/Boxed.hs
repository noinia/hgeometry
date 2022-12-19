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
  ( Point
  , PointF(..)
  ) where

import HGeometry.Point.PointF
import HGeometry.Vector.Boxed

--------------------------------------------------------------------------------

-- | Optimized point type
type Point d r = PointF (Vector d r)
