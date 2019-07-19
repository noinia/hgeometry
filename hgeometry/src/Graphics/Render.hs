--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Render
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Some rendering functions for rendering (drawing) geometric objects
--
--------------------------------------------------------------------------------
module Graphics.Render where

import Data.Ext
import Control.Lens
import Data.Geometry.Point
import Data.Geometry.Triangle
import Data.Geometry.LineSegment
import Data.Geometry.Transformation
import Data.Geometry.Properties

--------------------------------------------------------------------------------
-- * Rendering functions

-- | Rendering function for a triangle.
--
renderTriangle :: Fractional r => Transformation 3 r -> Triangle 3 p r -> Triangle 2 p r
renderTriangle = renderWithTransform projectTriangle
  where
    projectTriangle (Triangle p q r) = Triangle (p&core %~ projectPoint)
                                                (q&core %~ projectPoint)
                                                (r&core %~ projectPoint)

-- | Render a point
renderPoint :: Fractional r => Transformation 3 r -> Point 3 r -> Point 2 r
renderPoint = renderWithTransform projectPoint

-- | Renders a line segment
renderLineSegment :: Fractional r => Transformation 3 r -> LineSegment 3 p r -> LineSegment 2 p r
renderLineSegment = renderWithTransform project
  where
    project s = s&endPoints.core %~ projectPoint



-- | Generic Rendering Function
renderWithTransform           :: (Fractional r, IsTransformable g, Dimension g ~ 3, NumType g ~ r)
                              => (g -> g') -- ^ Projection function
                              -> Transformation 3 r -- ^ The camera transform
                              -> g  -- ^ The thing we wish to transform
                              -> g'
renderWithTransform project t = project . transformBy t
