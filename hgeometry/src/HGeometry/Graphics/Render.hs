--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Graphics.Render
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Data type to represent a camera and some functions for working with it.
--
-- Some functions that help rendering 3D objects
--
--------------------------------------------------------------------------------
module HGeometry.Graphics.Render
  ( ProjectedTriangle(ProjectedTriangle), triangle3
  , toTriangle2
  , renderTriangles
  ) where

import Data.Functor.Apply
import Data.Traversable
import Data.Coerce
import Control.Lens
import HGeometry.Graphics.Camera
import HGeometry.Triangle
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Ext
import HGeometry.Transformation

--------------------------------------------------------------------------------

-- | Render a scene; i..e a set of triangles
--
-- this intermediately uses doubles to apply the camera transform
renderTriangles        :: forall triangle point r set.
                          ( Functor set
                          , Triangle_ triangle point, Point_ point 3 r
                          , Real r, Fractional r
                          )
                       => Camera Double
                       -> set triangle
                       -> set (ProjectedTriangle r :+ triangle)
renderTriangles camera = fmap $ \orig@(Triangle_ a b c) ->
                         ProjectedTriangle (Triangle (f a) (f b) (f c)) :+ orig
  where
    f = f2 . transformBy (cameraTransform camera) . f3

    f2 :: Point 3 Double -> Point 3 r
    f2 = over coordinates realToFrac

    f3   :: point -> Point 3 Double
    f3 p = over coordinates realToFrac (p^.asPoint)
    -- TODO: clean up


-- | Represent the projection of a 3D triangle in 2D space.  i.e. this
-- triangle acts as a triangle in R^2, but also has the information
-- from where it came from.
newtype ProjectedTriangle r = ProjectedTriangle {_triangle3 :: Triangle (Point 3 r) }
  deriving stock (Show,Eq)

instance Functor ProjectedTriangle where
  fmap = fmapDefault

instance Foldable ProjectedTriangle where
  foldMap = foldMapDefault

instance Traversable ProjectedTriangle where
  traverse f (ProjectedTriangle t) =
    ProjectedTriangle <$> unwrapApplicative (t&vertices.coordinates %%~ WrapApplicative . f)

-- | Access the 3D triagnle
triangle3 :: Iso' (ProjectedTriangle r) (Triangle (Point 3 r))
triangle3 = coerced

type instance NumType   (ProjectedTriangle r) = r
type instance Dimension (ProjectedTriangle r) = 2

-- | Renders a Projected Triangle as a 2D Triangle
toTriangle2 :: ProjectedTriangle r -> Triangle (Point 2 r)
toTriangle2 = fmap projectPoint . coerce
