module HGeometry.Ball.BoundaryPoints
  ( BallByPoints
  , diskFromPoints
  ) where

import Control.Lens
import HGeometry.Ball.Class
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | A ball specified by d+1 points which form a CCW simplex.
newtype BallByPoints point = BoundaryPoints (Vector (1 + Dimension point) point)
                           deriving stock (Show,Eq,Functor)
                           deriving newtype (Foldable1)

-- smart constructor that makes sure the points are given in the right orientation
diskFromPoints       :: (Point_ point 2 r)
                     => point -> point -> point -> BallByPoints point
diskFromPoints a b c = undefined

type instance Dimension (BallByPoints point) = Dimension point
type instance NumType   (BallByPoints point) = NumType   point


instance HasInBall (BallByPoints point) 2 r where
  inBall q (BoundaryPoints (Vector3 a b c)) = let v (Point2_ x y) = Vector4 x y (x*x+y*y) 1
                                                  m = matrixFromRows $ Vector4 (v a)
                                                                               (v b)
                                                                               (v b)
                                                                               (v q)
                                              in case det m `compare` 0 of
                                                   LT -> Inside
                                                   EQ -> OnBoundary
                                                   GT -> Outside
    -- see:
    -- Lecture Notes on Geometric Robustness
    -- Jonathan Richard Shewchuk jrs@cs.berkeley.edu
    -- April 15, 2013
