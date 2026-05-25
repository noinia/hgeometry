--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HalfSpace.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Class for modelling Halfspaces
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfSpace.Class
  ( HalfSpace_(..)
  , HalfPlane_
  , inHalfSpace
  ) where

import Control.Lens
import HGeometry.Ext
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Sign
import HGeometry.Point.Class
import HGeometry.Boundary
import HGeometry.HyperPlane.Class

--------------------------------------------------------------------------------

-- | Types modelling halfspaces.
class ( Dimension halfSpace ~ d, Dimension (BoundingHyperPlane halfSpace d r) ~ d
      , NumType halfSpace ~ r,   NumType (BoundingHyperPlane halfSpace d r) ~ r
      )
       => HalfSpace_ halfSpace d r | halfSpace -> d,
                                     halfSpace -> r where
  type BoundingHyperPlane halfSpace d r
  -- removed the 'HyperPlane_' constraint on BoundingHyperPlane. Since at least in R^1
  -- it's useful to not have it.
  -- HyperPlane_ (BoundingHyperPlane halfSpace d r) d r

  -- | Access the bounding hyperplane
  boundingHyperPlane :: Lens' halfSpace (BoundingHyperPlane halfSpace d r)

  -- | Lens to access the sign of the halfspace.
  halfSpaceSign :: Lens' halfSpace Sign

-- | Type synonym for halfplanes in R^2
type HalfPlane_ halfPlane r = HalfSpace_ halfPlane 2 r


-- class Line_ (BoundingLine halfPlane r) 2 r
--       => HalfPlane_ halfPlane r | halfPlane -> r where
--   type BoundingLine halfPlane r

--   -- | Lens to access the boundin gline of a halfspace
--   boundingLine :: Lens' halfPlane (BoundingLine halfPlane r)

--   -- | Get the normal vector into the halfplane
--   normalIntoHalfPlane :: halfPlane -> Vector 2 r

instance HalfSpace_ core d r => HalfSpace_ (core :+ extra) d r where
  type BoundingHyperPlane (core :+ extra) d r = BoundingHyperPlane core d r
  boundingHyperPlane = core.boundingHyperPlane
  halfSpaceSign = core.halfSpaceSign



--------------------------------------------------------------------------------


-- | Test if a point lies inside a halfspace
inHalfSpace     :: ( Point_ point d r, Ord r, Num r
                   , HalfSpace_ halfSpace d r
                   , HyperPlane_ (BoundingHyperPlane halfSpace d r) d r
                   )
                => point -> halfSpace -> PointLocationResult
inHalfSpace q h = case q `onSideTest` (h^.boundingHyperPlane) of
                    LT -> case h^.halfSpaceSign of
                            Negative -> Inside
                            Positive -> Outside
                    GT -> case h^.halfSpaceSign of
                            Negative -> Outside
                            Positive -> Inside
                    EQ -> OnBoundary
