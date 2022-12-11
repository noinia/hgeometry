{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.NonVertical
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Non-vertical hyperplanes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.NonVertical
  ( NonVerticalHyperPlane(NonVerticalHyperPlane, Plane)
  ) where

import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import HGeometry.Properties
import HGeometry.Vector
-- import GHC.TypeLits

--------------------------------------------------------------------------------

-- | A non-vertical Hyperplane described by \( x_d = a_d + \sum_{i=1}^{d-1}
-- a_i * x_i \) where \(\langle a_1,..,a_d \rangle \) are the
-- coefficients of te hyperplane.
newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d
type instance VectorFor (NonVerticalHyperPlane d r) = Vector d r

instance ( MkHyperPlaneConstraints d r
         , Fractional r
         ) => HyperPlane_ (NonVerticalHyperPlane d r) d r where

  -- | pre: the last component is not zero
  hyperPlaneFromEquation e = NonVerticalHyperPlane $ a ^/ (-x)
    where
      (a,x) = unsnoc e

instance ( MkHyperPlaneConstraints d r
         , Fractional r
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where

--------------------------------------------------------------------------------
-- * Specific 3D Functions

-- | Constructs a Plane in R^3 for the equation z = ax + by + c
pattern Plane       :: ConstructableVector_ (VectorFamily 3 r) 3 r
                    => r -> r -> r -> NonVerticalHyperPlane 3 r
pattern Plane a b c = NonVerticalHyperPlane (Vector3 a b c)
