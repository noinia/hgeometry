--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Transformation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Geometry.Transformation
  ( Transformation(Transformation)
  , transformationMatrix
  , (|.|), identity, inverseOf

  , IsTransformable(..)
  , transformAllBy
  , transformPointFunctor

  , translation, scaling, uniformScaling

  , translateBy, scaleBy, scaleUniformlyBy

  , rotateTo

  , skewX, rotation, reflection, reflectionV, reflectionH

  , fitToBox
  , fitToBoxTransform
  ) where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Box.Internal (Rectangle, IsBoxable)
import qualified Data.Geometry.Box.Internal as Box
import           Data.Geometry.Properties
import           Data.Geometry.Point
import           Data.Geometry.Transformation.Internal
import           Data.Geometry.Vector
--------------------------------------------------------------------------------

-- | Given a rectangle r and a geometry g with its boundingbox,
-- transform the g to fit r.
fitToBox     :: forall g r q.
                ( IsTransformable g, IsBoxable g, NumType g ~ r, Dimension g ~ 2
                , Ord r, Fractional r
                ) => Rectangle q r -> g -> g
fitToBox r g = transformBy (fitToBoxTransform r g) g

-- | Given a rectangle r and a geometry g with its boundingbox,
-- compute a transformation can fit g to r.
fitToBoxTransform     :: forall g r q. ( IsTransformable g, IsBoxable g
                                       , NumType g ~ r, Dimension g ~ 2
                                       , Ord r, Fractional r
                      ) => Rectangle q r -> g -> Transformation 2 r
fitToBoxTransform r g = translation v2 |.| uniformScaling lam |.| translation v1
  where
    b = Box.boundingBox g
    v1  :: Vector 2 r
    v1  = negate <$> b^.to Box.minPoint.core.vector
    v2  = r^.to Box.minPoint.core.vector
    lam = minimum $ (/) <$> Box.size r <*> Box.size b
