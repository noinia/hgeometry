--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Transformation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Transformation
  ( Transformation(Transformation)
  , transformationMatrix
  , (|.|), identity, inverseOf

  , IsTransformable(..)

  , translation, scaling, uniformScaling

  , translateBy, scaleBy, scaleUniformlyBy

  , rotateTo

  , skewX, rotation, reflection, reflectionV, reflectionH

  -- , fitToBox
  -- , fitToBoxTransform
  ) where

import           Control.Lens
import           Data.Ext
-- import           HGeometry.Box.Internal (Rectangle, IsBoxable)
import           HGeometry.Box.Class
import qualified HGeometry.Box.Class as Box
import           HGeometry.Properties
import           HGeometry.Point
import           HGeometry.Transformation.Internal
import           HGeometry.Vector
--------------------------------------------------------------------------------

{-

-- | Given a rectangle r and a geometry g with its boundingbox,
-- transform the g to fit r.
fitToBox     :: forall g r rectangle.
                ( IsTransformable g
                , IsBoxable g
                , NumType g ~ r, Dimension g ~ 2
                , Ord r, Fractional r
                , Rectangle_ rectangle r
                ) => rectangle -> g -> g
fitToBox r g = transformBy (fitToBoxTransform r g) g

-- | Given a rectangle r and a geometry g with its boundingbox,
-- compute a transformation can fit g to r.
fitToBoxTransform     :: forall g r rectangle.
                         ( IsTransformable g, IsBoxable g
                         , NumType g ~ r, Dimension g ~ 2
                         , Ord r, Fractional r
                         , Rectangle_ rectangle r
                      ) => rectangle -> g -> Transformation 2 r
fitToBoxTransform r g = translation v2 |.| uniformScaling lam |.| translation v1
  where
    b = boundingBox g
    v1  :: Vector 2 r
    v1  = negate <$> b^.minPoint.vector
    v2  = r^.minPoint.vector
    lam = minimum $ (/) <$> Box.size r <*> Box.size b

-}
