--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ellipse
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing an Ellipse.
--------------------------------------------------------------------------------
module HGeometry.Ellipse(
    Ellipse(Ellipse)
  , affineTransformation
  , ellipseMatrix
  , unitEllipse
  , circleToEllipse, ellipseToCircle, _EllipseCircle
  ) where

import           Control.Lens hiding (elements)
import           HGeometry.Ball
import           HGeometry.Matrix
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | A type representing planar ellipses
newtype Ellipse r = Ellipse { _affineTransformation :: Transformation 2 r }
                   deriving (Show,Eq) -- ,Functor,Foldable,Traversable)

affineTransformation :: Iso (Ellipse r) (Ellipse s) (Transformation 2 r) (Transformation 2 s)
affineTransformation = iso _affineTransformation Ellipse

type instance Dimension (Ellipse r) = 2
type instance NumType   (Ellipse r) = r

instance Functor Ellipse where
  fmap = over $ affineTransformation.transformationMatrix.elements

instance Foldable Ellipse where
  foldMap = foldMapOf $ affineTransformation.transformationMatrix.elements

instance Traversable Ellipse where
  traverse f e = e&elements' %%~ f
    where
      elements' = cloneTraversal $ affineTransformation.transformationMatrix.elements


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
_EllipseCircle :: (Radical.Radical r, Eq r) => Prism' (Ellipse r) (Circle (Point 2 r))
_EllipseCircle = prism' circleToEllipse ellipseToCircle

-- | Try to Convert a circle into an ellipse. Returns a Nothing if the
-- ellipse is not an actual circle.
ellipseToCircle   :: (Num r, Eq r) => Ellipse r -> Maybe (Circle (Point 2 r))
ellipseToCircle e = case e^.ellipseMatrix of
      Matrix (Vector3 (Vector3 sx 0 x)
                      (Vector3 0 sy y)
                      (Vector3 0 0  1)
             )
           | sx == sy -> Just $ Circle (Point2 x y) (sx*sx)
      _               -> Nothing

-- | Convert a circle to an ellipse
circleToEllipse                            :: ( Radical.Radical r
                                              , Point_ point 2 r
                                              ) => Circle point -> Ellipse r
circleToEllipse (Circle p rr) =
  Ellipse $ translation (p^.vector) |.| uniformScaling (Radical.sqrt rr)
