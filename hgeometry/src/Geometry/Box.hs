{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Box
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Orthogonal \(d\)-dimensiontal boxes (e.g. rectangles)
--
--------------------------------------------------------------------------------
module Geometry.Box
  ( Box(Box)
  , box, fromExtent, fromCenter
  , grow
  , minPoint, maxPoint

  , centerPoint

  , inBox, inBox', insideBox
  , extent, size
  , widthIn, widthIn'

  , Rectangle
  , width, height


  , IsBoxable(..)
  , boundingBoxList, boundingBoxList'

  , module Geometry.Box.Corners
  , module Geometry.Box.Sides
  ) where

import Control.DeepSeq
import Geometry.Box.Corners
import Geometry.Box.Internal
import Geometry.Box.Sides
import Geometry.Vector
import Geometry.Point
import Geometry.Boundary

--------------------------------------------------------------------------------

deriving instance (NFData p, NFData r, Arity d) => NFData (Box d p r)


-- | Compute whether the point lies inside, on the boundary of, or
-- outside the box.
inBox' :: (Arity d, Ord r) => Point d r -> Box d p r -> PointLocationResult
q `inBox'` b | q `insideBox` b = Inside
             | q `inBox`     b = OnBoundary
             | otherwise       = Outside
