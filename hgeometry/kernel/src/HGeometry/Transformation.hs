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
import           Data.Semialign
import           HGeometry.Box.Boxable
import           HGeometry.Box.Class
import qualified HGeometry.Box.Class as Box
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Transformation.Internal
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Given a box r and a geometry g with its boundingbox,
-- transform the g to fit r.
fitToBox     :: forall box d g point r.
                ( NumType g ~ r, Dimension g ~ d
                , IsTransformable g
                , IsBoxable g
                , Box_ box point
                , Point_ point d r
                , Ord r, Fractional r
                , TransformationConstraints d r
                , Functor (Vector d), Zip (Vector d)
                ) => box -> g -> g
fitToBox r g = transformBy (fitToBoxTransform r g) g
{-# INLINE fitToBox #-}

-- | Given a box r and a geometry g with its boundingbox,
-- compute a transformation can fit g to r.
fitToBoxTransform     :: forall box d g point r.
                         ( NumType g ~ r, Dimension g ~ d
                         , IsTransformable g
                         , IsBoxable g
                         , Box_ box point
                         , Point_ point d r
                         , Ord r, Fractional r
                         , TransformationConstraints d r
                         , Functor (Vector d), Zip (Vector d)
                         ) => box -> g -> Transformation d r
fitToBoxTransform r g = translation v2 |.| uniformScaling lam |.| translation v1
  where
    b = boundingBox g
    v1  :: Vector d r
    v1  = negated $ b^.minPoint.vector
    v2  = r^.minPoint.vector
    lam = minimum1Of components $ liftI2 (/) (Box.size r) (Box.size b)
{-# INLINE fitToBoxTransform #-}
