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
  , toCounterClockwiseTriangle
  , triangleSignedArea2X
  , intersectingHalfPlanes
  , toBarricentric, fromBarricentric
  ) where

import Control.Lens
import Data.Ord
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Point
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Triangle


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


-- | Computes the double-signed area of a triangle
triangleSignedArea2X                   :: ( Num r
                                          , Point_ point 2 r
                                          , Triangle_ triangle point
                                          ) => triangle -> r
triangleSignedArea2X (Triangle_ a b c) = sum [ p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord
                                             | (p,q) <- edges
                                             ]
  where
    edges = [(a,b),(b,c),(c,a)]
{-# INLINE triangleSignedArea2X #-}

-- | Make sure that the triangles vertices are given in counter clockwise order
--
-- >>> let t = Triangle origin (Point2 0 (-1)) (Point2 (-1) 0) :: Triangle (Point 2 Int)
-- >>> toCounterClockwiseTriangle t
-- Triangle (Point2 0 0) (Point2 (-1) 0) (Point2 0 (-1))
toCounterClockwiseTriangle :: ( Num r, Eq r
                              , Point_ point 2 r
                              , Triangle_ triangle point
                              ) => triangle -> triangle
toCounterClockwiseTriangle t@(Triangle_ a b c)
    | isCounterClockwise t = t
    | otherwise            = mkTriangle a c b
  where
    isCounterClockwise = (\x -> x == abs x) . triangleSignedArea2X

-- | Get the three halfplanes such that the triangle is the intersection of those
-- halfspaces.
--
-- >>> let t = Triangle origin (Point2 0 (-1)) (Point2 (-1) 0) :: Triangle (Point 2 Int)
-- >>> mapM_ print $ intersectingHalfPlanes t
-- HalfSpace Negative (HyperPlane [0,0,1])
-- HalfSpace Positive (HyperPlane [-1,-1,-1])
-- HalfSpace Negative (HyperPlane [0,-1,0])
intersectingHalfPlanes :: ( Triangle_ triangle point
                          , Point_ point 2 r
                          , Num r, Ord r
                          )
                       => triangle
                       -> Vector 3 (HalfSpace 2 r)
intersectingHalfPlanes (toCounterClockwiseTriangle -> Triangle_ u v w) =
    Vector3 (leftPlane u v) (leftPlane v w) (leftPlane w u)
  where
    leftPlane p q = let dir = if (p^.xCoord, Down $ p^.yCoord) <= (q^.xCoord, Down $ q^.yCoord)
                                 -- for verical planes, the left side counts as below
                              then Positive else Negative
                    in HalfSpace dir . hyperPlaneThrough $ Vector2 p q


-- | Given a point q and a triangle, q inside the triangle, get the baricentric
-- cordinates of q
toBarricentric                                   :: ( Fractional r
                                                    , Point_ point 2 r
                                                    , Triangle_ triangle point
                                                    )
                                                 => point -> triangle
                                                 -> Vector 3 r
toBarricentric (Point2_ qx qy) (Triangle_ a b c) = Vector3 alpha beta gamma
  where
    Point2_ ax ay = a
    Point2_ bx by = b
    Point2_ cx cy = c

    dett  = (by - cy)*(ax - cx) + (cx - bx)*(ay - cy)

    alpha = ((by - cy)*(qx - cx) + (cx - bx)*(qy - cy)) / dett
    beta  = ((cy - ay)*(qx - cx) + (ax - cx)*(qy - cy)) / dett
    gamma = 1 - alpha - beta
    -- see https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates

-- | Given a vector of barricentric coordinates and a triangle, get the
-- corresponding point in the same coordinate sytsem as the vertices of the
-- triangle.
fromBarricentric                                   :: ( Triangle_ triangle point
                                                      , Point_ point d r
                                                      , Num r
                                                      )
                                                   => Vector 3 r
                                                   -> triangle
                                                   -> Point d r
fromBarricentric (Vector3 a b c) (Triangle_ p q r) = let f = view vector in
                                                       Point $ a *^ f p ^+^ b *^ f q ^+^ c *^ f r
