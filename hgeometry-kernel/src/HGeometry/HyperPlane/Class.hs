module HGeometry.HyperPlane.Class
  ( HyperPlane_(..)
  , NonVerticalHyperPlane_(..)
  ) where

import GHC.TypeNats
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Vector.Class

--------------------------------------------------------------------------------


-- | A class to represent hyperplanes in d-dimensional space.
class ( NumType hyperPlane ~ r
      , Dimension hyperPlane ~ d
      ) => HyperPlane_ hyperPlane d r | hyperPlane -> d
                                      , hyperPlane -> r where

  -- | Constructs the hyperplane through d points
  hyperPlaneTrough :: (Point_ point d r) => Vector d point -> hyperPlane

  -- | A hyperplane \(h) has coefficients \(a_i \in \mathbb{R}\) so that
  -- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
  -- \(a_0 = \sum_i=1^d) a_i*p_i
  --
  -- this fuction returns the vector of these coefficients \(\langle a_0,..,a_d \rangle\)
  hyperplaneCoefficients :: hyperPlane -> Vector (d+1) r




class HyperPlane_ hyperPlane d r => NonVerticalHyperPlane_ hyperPlane d r where
  -- | Get the coordinate in dimesnion $d$ of the hyperplane at the given position.
  evalAt :: Point_ point (d-1) r => point -> hyperPlane -> r


-- instance NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where
--   evalAt p h = ((p^.vector) |> 1) `dot` h^.coefficients









-- intersectionPoint                           :: HyperPlane_ line 2 r
--                                             => line -> line -> Maybe (Point 2 r)
-- intersectionPoint (Line a1 b1) (Line a2 b2) = undefined
--   where
