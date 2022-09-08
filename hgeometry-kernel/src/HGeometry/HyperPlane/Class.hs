module HGeometry.HyperPlane.Class
  ( HyperPlane_(..)
  ) where

import GHC.TypeNats
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------


-- | A class to represent hyperplanes in d-dimensional space.
class ( NumType hyperPlane ~ r
      , Dimension hyperPlane ~ d
      ) => HyperPlane_ hyperPlane d r | hyperPlane -> d
                                      , hyperPlane -> r where

  -- | Constructs the hyperplane through d points
  hyperPlaneTrough :: Point_ point d r => Vector d point -> hyperPlane

  -- | A hyperplane \(h) has coefficients \(a_i \in \mathbb{R}\) so that
  -- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
  -- \(a_0 = \sum_i=1^d) a_i*p_i
  --
  -- this fuction returns the vector of these coefficients \(\langle a_0,..,a_d \rangle\)
  hyperplaneCoefficients :: hyperPlane -> Vector (d+1) r



-- | A non-vertical Hyperplane described by \( x_d = a_d + \sum_{i=1}^{d-1}
-- a_i * x_i \) where \(\langle a_1,..,a_d \rangle \) are the
-- coefficients of te hyperplane.
newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d

instance HyperPlane_ (NonVerticalHyperPlane d r) d r where
  -- hyperPlaneTrough = undefined
  -- hyperplaneCoefficients (NonVerticalHyperPlane v) = undefined


pattern Line     :: r -> r -> NonVerticalHyperPlane 2 r
pattern Line a b = NonVerticalHyperPlane (Vector2 a b)

pattern Plane       :: r -> r -> r -> NonVerticalHyperPlane 3 r
pattern Plane a b c = NonVerticalHyperPlane (Vector3 a b c)
