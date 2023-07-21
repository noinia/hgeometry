{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Duality
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Standard duality between points and non-vertical hyperplanes (or lines in R^2)
--
--------------------------------------------------------------------------------
module HGeometry.Duality
  ( dualPoint
  , dualHyperPlane
  , dualLine

  , liftPointToPlane
  ) where

import Control.Lens
import GHC.TypeNats
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.NonVertical
import HGeometry.Line.LineEQ
import HGeometry.Point
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Compute the point dual to a non-vertical hyperplane
--
-- >>> dualPoint (LineEQ 10 20)
-- Point2 10 (-20)
dualPoint :: forall hyperPlane d r. (NonVerticalHyperPlane_ hyperPlane d r, Num r, 1 <= d)
          => hyperPlane -> Point d r
dualPoint = Point . over (component @(d-1)) negate . view hyperPlaneCoefficients

-- | Compute the dual of a hyperplane
--
-- >>> dualHyperPlane (Point20 10 (-20))
-- NonVerticalHyperPlane (Vector2 10 20)
dualHyperPlane :: forall point d r. (Point_ point d r, Num r, 1 <= d)
               => point -> NonVerticalHyperPlane d r
dualHyperPlane = NonVerticalHyperPlane . over (component @(d-1)) negate . view vector

-- | Compute the line dual to a point
--
-- >>> dualLine (Point20 10 (-20))
-- LineEQ 10 20
dualLine :: (Point_ point 2 r, Num r) => point -> LineEQ r
dualLine = MkLineEQ . dualHyperPlane


--------------------------------------------------------------------------------

-- | The standard lifting transform, that lifts a point to the plane
-- tangent to the unit hyperboloid.
liftPointToPlane               :: (Point_ point 2 r, Num r) => point -> NonVerticalHyperPlane 3 r
liftPointToPlane (Point2_ x y) = Plane (2*x) (2*y) (negate $ x*x + y*y)
