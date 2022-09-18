{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module HGeometry.HyperPlane.Internal
  ( HyperPlane(..)
  , MkHyperPlaneConstraints
  ) where

import GHC.TypeNats
import Data.Type.Ord
import HGeometry.HyperPlane.Class
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A Hyperplane in d-dimensions, described by
--
-- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
-- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
newtype HyperPlane d r = HyperPlane (Vector (d+1) r)

type instance NumType   (HyperPlane d r) = r
type instance Dimension (HyperPlane d r) = d
type instance VectorFor (HyperPlane d r) = Vector d r

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | Constraints on d needed to be able to construct hyperplanes; pretty much all of
-- these are satisfied by default, it is just that the typechecker does not realize that.
type MkHyperPlaneConstraints d =
  (Arity d, Arity (d+1)
  , d < d+1
  )

instance MkHyperPlaneConstraints d => HyperPlane_ (HyperPlane d r) d r where
  type EquationFor (HyperPlane d r) = Vector (d+1) r
  hyperPlaneEquation (HyperPlane v) = v
  hyperPlaneFromEquation = HyperPlane


--  hyperPlaneTrough pts = fromPointAndNormal p0 n
--    where
--      p0 = pts^.component @0
--      -- (p0, pts') = uncons pts
--      -- vecs = (.-. p0) <$> pts'
--      n = error "hyperPlaneTrhough: undefined!"
