{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HyperPlane.Internal
  ( HyperPlane(..)
  , MkHyperPlaneConstraints
  , fromPointAndNormal
  ) where

import Control.Lens hiding (snoc,cons,unsnoc,uncons)
import GHC.TypeNats
import Data.Type.Ord
import HGeometry.HyperPlane.Class
import HGeometry.Point.Class
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

--------------------------------------------------------------------------------

-- | Construct a Hyperplane from a point and a normal.
fromPointAndNormal     :: ( Point_ point d r
                          , vector ~ Diff_ point
                          , Num r
                          , Arity (d+1)
                          )
                       => point -> vector -> HyperPlane d r
fromPointAndNormal q n = HyperPlane $ cons a0 n
  where
    a0 = negate $ (q^.vector) `dot` n

--------------------------------------------------------------------------------

-- | Constraints on d needed to be able to construct hyperplanes; pretty much all of
-- these are satisfied by default, it is just that the typechecker does not realize that.
type MkHyperPlaneConstraints d =
  (Arity d, Arity (d+1), d <= d+1, d < d+1,KnownNat ((d+1)-d), 0 < d+1, 0 < d)

instance MkHyperPlaneConstraints d => HyperPlane_ (HyperPlane d r) d r where
  hyperPlaneEquation (HyperPlane v) = v

--  hyperPlaneTrough pts = fromPointAndNormal p0 n
--    where
--      p0 = pts^.component @0
--      -- (p0, pts') = uncons pts
--      -- vecs = (.-. p0) <$> pts'
--      n = error "hyperPlaneTrhough: undefined!"
