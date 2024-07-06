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
  , TransformationConstraints
  , DefaultTransformByConstraints
  , OptMatrix_

  , translation, scaling, uniformScaling

  , translateBy, scaleBy, scaleUniformlyBy

  , rotateTo

  , skewX, rotation, reflection, reflectionV, reflectionH

  , fitToBox
  , fitToBoxTransform
  ) where

import           Control.Lens hiding ((<.>))
import           HGeometry.Box.Boxable
import           HGeometry.Box.Class
import qualified HGeometry.Box.Class as Box
import           HGeometry.Point
import           HGeometry.Matrix
import           HGeometry.Properties
import           HGeometry.Transformation.Internal
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Given a rectangle r and a geometry g with its boundingbox,
-- transform the g to fit r.
fitToBox     :: forall rectangle g point r.
                ( NumType g ~ r, Dimension g ~ 2
                , IsTransformable g
                , IsBoxable g
                , Rectangle_ rectangle point
                , Point_ point 2 r
                , Ord r, Fractional r
                -- , HasComponents (Vector 2 (ClosedInterval r)) (Vector 2 r)
                ) => rectangle -> g -> g
fitToBox r g = transformBy (fitToBoxTransform r g) g
{-# INLINE fitToBox #-}

-- | Given a rectangle r and a geometry g with its boundingbox,
-- compute a transformation can fit g to r.
fitToBoxTransform     :: forall rectangle g point r.
                         ( NumType g ~ r, Dimension g ~ 2
                         , IsTransformable g
                         , IsBoxable g
                         , Rectangle_ rectangle point
                         , Point_ point 2 r
                         , Ord r, Fractional r
                         ) => rectangle -> g -> Transformation 2 r
fitToBoxTransform r g = translation v2 |.| uniformScaling lam |.| translation v1
  where
    b = boundingBox g
    v1  :: Vector 2 r
    v1  = negated $ b^.minPoint.vector
    v2  = r^.minPoint.vector
    lam = minimum1Of components $ liftI2 (/) (Box.size r) (Box.size b)
{-# INLINE fitToBoxTransform #-}
