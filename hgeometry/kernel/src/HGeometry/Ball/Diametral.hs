--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ball.Diametral
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A ball represented by its two diametral points
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Ball.Diametral
  ( DiametralBall(MkDiametralBall,DiametralPoints)
  ) where

import Control.Lens
import Data.Foldable1
import HGeometry.Ball.Class
import HGeometry.Boundary
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Intersection

--------------------------------------------------------------------------------

-- | A ball defined by its two diametral points
newtype DiametralBall point = MkDiametralBall (Vector 2 point)
                            deriving stock (Show,Eq,Functor,Foldable,Traversable)
                            deriving newtype (Foldable1)

pattern DiametralPoints     :: point -> point -> DiametralBall point
pattern DiametralPoints p q = MkDiametralBall (Vector2 p q)
{-# COMPLETE DiametralPoints #-}

type instance Dimension (DiametralBall point) = Dimension point
type instance NumType   (DiametralBall point) = NumType   point

instance Traversable1 DiametralBall where
  traverse1 f (MkDiametralBall v) = MkDiametralBall <$> traverse1 f v

--------------------------------------------------------------------------------

instance (Point_ point d r, Fractional r, Has_ Metric_ d r
         ) => Ball_ (DiametralBall point) (Point d r) where
  squaredRadius = to $ \(DiametralPoints p q) -> quadrance $ (p .-. q) ^/ 2

--------------------------------------------------------------------------------
-- * Point in ball

instance (Point_ point d r, Has_ Metric_ d r) => HasInBall (DiametralBall point) where
  inBall q (DiametralPoints a b) = let a' = a^.vector
                                       b' = b^.vector
                                       v = 2 *^ (q^.vector) ^-^ (a' ^+^ b')
                                       w = a' ^-^ b'
                                   in case (v `dot` v) `compare` (w `dot` w) of
                                        LT -> Inside
                                        EQ -> OnBoundary
                                        GT -> Outside
    -- main idea: solve: ||q-c||^2 <= r^2
    -- since we have c = (a+b)/2, and r=|a-b|/2
    -- we essentially avoid the division by using (2^2)*r^2 = (2r)^2 = (a-b)^2
    -- simialrly on the left side.

type instance Intersection (Point d r) (DiametralBall point) = Maybe (Point d r)

instance ( Point_ point d r, Ord r, Num r, Has_ Metric_ d r
         ) => (Point d r) `HasIntersectionWith` (DiametralBall point) where
  intersects q b = q `inBall` b /= Outside

instance ( Point_ point d r
         , Ord r, Num r
         , Has_ Metric_ d r
         ) => (Point d r) `IsIntersectableWith` (DiametralBall point) where
  intersect q b | q `intersects` b = Just q
                | otherwise        = Nothing


--------------------------------------------------------------------------------

instance (Point_ point d r, Fractional r) => HasCenter  (DiametralBall point) (Point d r) where
  center = lens computeCenter (\ball c' -> let c = computeCenter ball
                                               v = c' .-. c
                                           in (.+^ v) <$> ball
                              ) -- shifts the center to the new position

-- | Computes the center of the ball
computeCenter :: (Point_ point d r, Fractional r) => DiametralBall point -> Point d r
computeCenter (DiametralPoints a b) = Point $ (a^.vector ^+^ b^.vector) ^/ 2
