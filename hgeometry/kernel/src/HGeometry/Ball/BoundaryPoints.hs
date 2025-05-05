--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ball.BoundaryPoints
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A ball in \(d\) dimensional space represented by \(d+1\) points on the boundary.
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Ball.BoundaryPoints
  ( BallByPoints, BallByPoints'(..)
  , diskFromPoints
  ) where

import Control.Lens
import Data.Foldable1
import GHC.TypeLits
import HGeometry.Ball.Class
import HGeometry.Boundary
import HGeometry.Intersection
import HGeometry.Matrix
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Linear.Matrix (det33)
import Linear.V3 (V3(..))

--------------------------------------------------------------------------------

-- | A ball specified by d+1 points which form a CCW simplex.
type BallByPoints point = BallByPoints' (1 + Dimension point) point

-- | A ball specified by k points which form a CCW simplex.
--
-- This is just a helper type to implement the BallByPoints type.
newtype BallByPoints' k point = BoundaryPoints (Vector k point)


deriving stock   instance Show (Vector k point) => Show (BallByPoints' k point)
deriving newtype instance Eq   (Vector k point) => Eq (BallByPoints' k point)
deriving newtype instance Ord  (Vector k point) => Ord (BallByPoints' k point)

deriving newtype instance Functor   (Vector k) => Functor   (BallByPoints' k)
deriving newtype instance Foldable  (Vector k) => Foldable  (BallByPoints' k)
deriving newtype instance Foldable1 (Vector k) => Foldable1 (BallByPoints' k)

instance Traversable (Vector k) => Traversable (BallByPoints' k) where
  traverse f (BoundaryPoints v) = BoundaryPoints <$> traverse f v
instance Traversable1 (Vector k) => Traversable1 (BallByPoints' k) where
  traverse1 f (BoundaryPoints v) = BoundaryPoints <$> traverse1 f v


-- | smart constructor that makes sure the points are given in the right orientation may
-- fail if the points are colinear
diskFromPoints       :: (Point_ point 2 r, Ord r, Num r)
                     => point -> point -> point -> Maybe (BallByPoints point)
diskFromPoints a b c = case ccw a b c of
                         CCW      -> Just . BoundaryPoints $ Vector3 c b a
                         CW       -> Just . BoundaryPoints $ Vector3 a b c
                         CoLinear -> Nothing

type instance Dimension (BallByPoints' k point) = Dimension point
type instance NumType   (BallByPoints' k point) = NumType   point

instance ( Point_ point d r, Fractional r, 1 <= k, Has_ Vector_ k point, Has_ Metric_ d r
         , HasCenter (BallByPoints' k point) (Point d r)
         ) => Ball_ (BallByPoints' k point) (Point d r) where
  squaredRadius = to $ \ball@(BoundaryPoints v) ->
                         let p = v^.xComponent.asPoint
                         in squaredEuclideanDist (ball^.center) p

--------------------------------------------------------------------------------
-- * In ball

instance Point_ point 2 r => HasInBall (BallByPoints' 3 point) where
  inBall q (BoundaryPoints (Vector3 a b c)) = let v (Point2_ x y) = Vector4 x y (x*x+y*y) 1
                                                  m = Matrix $ Vector4 (v a)
                                                                       (v b)
                                                                       (v c)
                                                                       (v q)
                                              in case det m `compare` 0 of
                                                   LT -> Inside
                                                   EQ -> OnBoundary
                                                   GT -> Outside
    -- see:
    -- Lecture Notes on Geometric Robustness
    -- Jonathan Richard Shewchuk jrs@cs.berkeley.edu
    -- April 15, 2013


type instance Intersection (Point d r) (BallByPoints' k point) = Maybe (Point d r)

instance ( Point_ point d r, Ord r, Num r, HasInBall (BallByPoints' k point)
         ) => (Point d r) `HasIntersectionWith` (BallByPoints' k point) where
  intersects q b = q `inBall` b /= Outside

instance ( Point_ point d r, Ord r, Num r, HasInBall (BallByPoints' k point)
         ) => (Point d r) `IsIntersectableWith` (BallByPoints' k point) where
  intersect q b | q `intersects` b = Just q
                | otherwise        = Nothing

--------------------------------------------------------------------------------

instance (Point_ point 2 r, Fractional r) => HasCenter  (BallByPoints' 3 point) (Point 2 r) where
  center = lens computeCenter (\ball c' -> let c = computeCenter ball
                                               v = c' .-. c
                                           in (.+^ v) <$> ball
                              ) -- shifts the center to the new position

-- | computes the center of a disk
computeCenter :: (Point_ point 2 r, Fractional r) => BallByPoints point -> Point 2 r
computeCenter (BoundaryPoints (Vector3 (Point2_ px py) (Point2_ qx qy) (Point2_ sx sy))) = c
  where
    f  x y = x^2 + y^2
    fx x y = V3 (f x y) y       1
    fy x y = V3 x       (f x y) 1

    xnom   = det33 $ V3 (fx px py) (fx qx qy) (fx sx sy)
    ynom   = det33 $ V3 (fy px py) (fy qx qy) (fy sx sy)

    denom  = (2 *) . det33 $ V3 (V3 px py 1) (V3 qx qy 1) (V3 sx sy 1)
    c      = Point2 (xnom / denom) (ynom / denom)


-- instance Ball_ (BallByPoints point) point where
--   squaredRadius
