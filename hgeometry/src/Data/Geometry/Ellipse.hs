{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Ellipse(
    Ellipse(Ellipse)
  , affineTransformation
  , ellipseMatrix
  , unitEllipse
  , circleToEllipse, ellipseToCircle, _EllipseCircle
  ) where

import Control.Lens
import Data.Ext
import Data.Geometry.Ball
import Data.Geometry.Matrix
import Data.Geometry.Transformation
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Vector

--------------------------------------------------------------------------------

-- | A typre representing planar ellipses
newtype Ellipse r = Ellipse { _affineTransformation :: Transformation 2 r }
                   deriving (Show,Eq,Functor,Foldable,Traversable)
makeLenses ''Ellipse

type instance Dimension (Ellipse r) = 2
type instance NumType   (Ellipse r) = r

instance Num r => IsTransformable (Ellipse r) where
  transformBy t (Ellipse t') = Ellipse $ t |.| t'


ellipseMatrix :: Iso (Ellipse r) (Ellipse s) (Matrix 3 3 r) (Matrix 3 3 s)
ellipseMatrix = affineTransformation.transformationMatrix

-- | Ellipse representing the unit circle
unitEllipse :: Num r => Ellipse r
unitEllipse = Ellipse $ Transformation identityMatrix

--------------------------------------------------------------------------------
-- | Converting between ellipses and circles

_EllipseCircle :: (Floating r, Eq r) => Prism' (Ellipse r) (Circle () r)
_EllipseCircle = prism' circleToEllipse ellipseToCircle

ellipseToCircle   :: (Num r, Eq r) => Ellipse r -> Maybe (Circle () r)
ellipseToCircle e = case e^.ellipseMatrix of
      Matrix (Vector3 (Vector3 sx 0 x)
                      (Vector3 0 sy y)
                      (Vector3 0 0  1)
             )
           | sx == sy -> Just $ Circle (ext $ Point2 x y) (sx*sx)
      _               -> Nothing

circleToEllipse                            :: Floating r => Circle p r -> Ellipse r
circleToEllipse (Circle (Point v :+ _) rr) = Ellipse $ translation v |.| uniformScaling (sqrt rr)
