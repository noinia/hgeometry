{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Box
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Orthogonal \(d\)-dimensiontal boxes (e.g. rectangles)
--
--------------------------------------------------------------------------------
module Data.Geometry.Box
  ( module Data.Geometry.Box.Internal
  , module Data.Geometry.Box.Corners
  , module Data.Geometry.Box.Sides
  , inBox'
  ) where

import Control.DeepSeq
import Data.Geometry.Box.Corners
import Data.Geometry.Box.Internal
import Data.Geometry.Box.Sides
import Data.Geometry.Vector
import Data.Geometry.Point
import Data.Geometry.Boundary

--------------------------------------------------------------------------------

deriving instance (NFData p, NFData r, Arity d) => NFData (Box d p r)


-- | Compute whether the point lies inside, on the boundary of, or
-- outside the box.
inBox' :: (Arity d, Ord r) => Point d r -> Box d p r -> PointLocationResult
q `inBox'` b | q `insideBox` b = Inside
             | q `inBox`     b = OnBoundary
             | otherwise       = Outside
