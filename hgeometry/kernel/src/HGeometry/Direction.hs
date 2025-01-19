{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Direction
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Compuations that have to do with directions
--
--------------------------------------------------------------------------------
module HGeometry.Direction
  ( CanComputeNormalVector(..)
  , uniformDirection
  , uniformUpwardDirectionWrt
  , module HGeometry.Direction.Cardinal
  ) where

import Control.Lens
import HGeometry.Ball
import HGeometry.Direction.Cardinal
import HGeometry.Number.Radical
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.Vector
import System.Random.Stateful

--------------------------------------------------------------------------------

class (r ~ NumType geom
      ) => CanComputeNormalVector geom r | geom -> r where
  {-# mINIMAL normalVectorAt #-}
  -- | Given a point on the object, and the object. Compute the outward normal vector at
  -- the given point. The normal is returned as a unit vector.
  --
  -- pre: the query point lies on (the surface of) the object. (This is not checked)
  normalUnitVectorAt     :: ( Point_ point d r
                            , Has_ Metric_ d r
                            , d ~ Dimension geom
                            , Radical r, Fractional r)
                         => point -> geom -> Vector d r
  normalUnitVectorAt q g = signorm $ normalVectorAt q g

  -- | Given a point on the object, and the object. Compute the outward normal vector at
  -- the given point. No Guarantees are given about the length of the resulting vector.
  --
  -- pre: the query point lies on (the surface of) the object. (This is not checked)
  normalVectorAt :: ( Point_ point d r, d ~ Dimension geom, Num r)
                 => point -> geom -> Vector d r

instance Point_ center d r => CanComputeNormalVector (Ball center) r where
  normalVectorAt q b = (q^.asPoint) .-. (b^.center.asPoint)

instance Point_ vertex 3 r => CanComputeNormalVector (Triangle vertex) r where
  normalVectorAt _ (Triangle u v w) = cross (v .-. u) (w .-. u)
  -- FIXME: shoudn't we check the orientation of the triangle: I think it may point the
  -- wrong way now


--------------------------------------------------------------------------------
-- * Generating directions


-- | Generates a unit vector corresponding to a direction.
--
-- The general idea is to generate a random vector in the [-1,1]^d, if it lies inside the
-- unit ball, scale it to be length 1, otherwise, retry.
uniformDirection     :: ( StatefulGen gen m
                        , Fractional r, Ord r, Radical r, UniformRange r
                        , Has_ Metric_ d r
                        ) => gen -> m (Vector d r)
uniformDirection gen = go
  where
    generate' x = generate (const x)
    go = do v <- uniformRM (generate' (-1), generate' 1) gen
            if quadrance v <= 1 then pure $ signorm v
                                else go

-- | Generates an upward direction (with respect to the given normal/up vector) uniformly
-- at random.
uniformUpwardDirectionWrt            :: ( StatefulGen gen m
                                        , Fractional r, Ord r, Radical r, UniformRange r
                                        , Has_ Metric_ d r
                                        ) => Vector d r -> gen -> m (Vector d r)
uniformUpwardDirectionWrt normal gen = uniformDirection gen <&> \dir ->
  if (normal `dot` dir) >= 0 then dir else negated dir
