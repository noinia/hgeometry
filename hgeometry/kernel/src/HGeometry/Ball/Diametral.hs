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
module HGeometry.Ball.Diametral
  ( DiametralBall(MkDiametralBall,DiametralPoints)
  ) where

import Control.Lens
import HGeometry.Ball.Class
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | A ball defined by its two diametral points
newtype DiametralBall point = MkDiametralBall (Vector 2 point)
                            deriving stock (Show,Eq,Functor,Foldable,Traversable)
                            deriving newtype (Foldable1)


type instance Dimension (DiametralBall point) = Dimension point
type instance NumType   (DiametralBall point) = NumType   point

pattern DiametralPoints p q = MkDiametralBall (Vector2 p q)

instance HasInBall (DiametralBall point) 2 r where
  inBall q (DiametralPoints a b) = let Vector2 x y = 2 *^ q ^-^ (a^.vector ^+^ b^.vector)
                                   in case (x*x + y*y) `compare` 2*(squaredEuclideanDist a b) of
                                        LT -> Inside
                                        EQ -> OnBoundary
                                        GT -> Outside
