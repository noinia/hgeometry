--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Triangle.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types representing Triangles
--
--------------------------------------------------------------------------------
module HGeometry.Triangle.Class
  ( Triangle_(..), pattern Triangle_
  , intersectingHalfPlanes
  ) where

import           Control.Lens
-- import qualified Data.Foldable as F
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane
-- import           HGeometry.Intersection
import           HGeometry.Point
import           HGeometry.Properties (NumType, Dimension)
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Class representing triangles
class ( Point_   point (Dimension point) (NumType point)
      )
     => Triangle_ triangle point | triangle -> point where

  -- | Construct a triangle from its three vertices.
  mkTriangle :: point -> point -> point -> triangle

  -- | Lens to access the corners of the triangle.
  corners :: Lens' triangle (Vector 3 point)

    -- IndexedTraversal1' Int triangle point

-- | Constructs a triangle
pattern Triangle_ :: Triangle_ triangle point => point -> point -> point -> triangle
pattern Triangle_ u v w <- (view corners -> Vector3 u v w)
  where
    Triangle_ u v w = mkTriangle u v w
{-# COMPLETE Triangle_ #-}
{-# INLINE Triangle_ #-}

--------------------------------------------------------------------------------
-- * Two dimensional convenience functions

-- | Get the three halfplanes such that the triangle is the intersection of those
-- halfspaces.
intersectingHalfPlanes                    :: ( Triangle_ triangle point
                                             , Point_ point 2 r
                                             , Num r, Eq r
                                             )
                                          => triangle
                                          -> Vector 3 (HalfSpace 2 r)
intersectingHalfPlanes (Triangle_ u v w) = Vector3 (above u v) (above v w) (above w u)
  where
    above p q = HalfSpace . hyperPlaneThrough $ Vector2 p q
