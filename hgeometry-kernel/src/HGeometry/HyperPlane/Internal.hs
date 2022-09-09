module HGeometry.HyperPlane.Internal
  (

  ) where

import GHC.TypeNats
import HGeometry.HyperPlane.Class
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A non-vertical Hyperplane described by \( x_d = a_d + \sum_{i=1}^{d-1}
-- a_i * x_i \) where \(\langle a_1,..,a_d \rangle \) are the
-- coefficients of te hyperplane.
newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d

instance HyperPlane_ (NonVerticalHyperPlane d r) d r where
  -- hyperPlaneTrough = undefined
  -- hyperplaneCoefficients (NonVerticalHyperPlane v) = undefined


--------------------------------------------------------------------------------

pattern Line     :: r -> r -> NonVerticalHyperPlane 2 r
pattern Line a b = NonVerticalHyperPlane (Vector2 a b)

pattern Plane       :: r -> r -> r -> NonVerticalHyperPlane 3 r
pattern Plane a b c = NonVerticalHyperPlane (Vector3 a b c)
