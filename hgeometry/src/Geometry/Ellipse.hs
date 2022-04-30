{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Ellipse
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing an Ellipse.
--------------------------------------------------------------------------------
module Geometry.Ellipse(
    Ellipse(Ellipse)
  , affineTransformation
  , ellipseMatrix
  , unitEllipse
  , circleToEllipse, ellipseToCircle, _EllipseCircle
  ) where

import Control.Lens
import Data.Ext
import Geometry.Ball
import Geometry.Matrix
import Geometry.Transformation
import Geometry.Point
import Geometry.Properties
import Geometry.Vector

--------------------------------------------------------------------------------

-- | A type representing planar ellipses
newtype Ellipse r = Ellipse { _affineTransformation :: Transformation 2 r }
                   deriving (Show,Eq,Functor,Foldable,Traversable)
makeLenses ''Ellipse

type instance Dimension (Ellipse r) = 2
type instance NumType   (Ellipse r) = r

instance Num r => IsTransformable (Ellipse r) where
  transformBy t (Ellipse t') = Ellipse $ t |.| t'

-- | Get the matrix describing the ellipse.
ellipseMatrix :: Iso (Ellipse r) (Ellipse s) (Matrix 3 3 r) (Matrix 3 3 s)
ellipseMatrix = affineTransformation.transformationMatrix

-- | Ellipse representing the unit circle
unitEllipse :: Num r => Ellipse r
unitEllipse = Ellipse $ Transformation identityMatrix

--------------------------------------------------------------------------------
-- | Converting between ellipses and circles

-- | Prims to convert between an Ellipse and a Circle.
_EllipseCircle :: (Floating r, Eq r) => Prism' (Ellipse r) (Circle () r)
_EllipseCircle = prism' circleToEllipse ellipseToCircle

-- | Try to Convert a circle into an ellipse. Returns a Nothing if the
-- ellipse is not an actual circle.
ellipseToCircle   :: (Num r, Eq r) => Ellipse r -> Maybe (Circle () r)
ellipseToCircle e = case e^.ellipseMatrix of
      Matrix (Vector3 (Vector3 sx 0 x)
                      (Vector3 0 sy y)
                      (Vector3 0 0  1)
             )
           | sx == sy -> Just $ Circle (ext $ Point2 x y) (sx*sx)
      _               -> Nothing

-- | Convert a circle to an ellipse
circleToEllipse                            :: Floating r => Circle p r -> Ellipse r
circleToEllipse (Circle (Point v :+ _) rr) = Ellipse $ translation v |.| uniformScaling (sqrt rr)
