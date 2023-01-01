module HGeometry.SoS.Determinant where

import GHC.TypeLits
import HGeometry.Matrix
import HGeometry.Number.Real.Symbolic
import HGeometry.Sign
import HGeometry.Vector

--------------------------------------------------------------------------------0

-- | pre: computes the sign of the determinant
signDet   :: (HasDeterminant d, KnownNat d, OptAdditive_ d (Symbolic i r)
             , Ord i, OptVector_ d (Symbolic i r), OptVector_ d (Vector d (Symbolic i r))
             , Num r, Ord r) => Matrix d d (Symbolic i r) -> Sign
signDet m = case det m `compare` 0 of
              LT -> Negative
              GT -> Positive
              EQ -> error "signDet: determinant is zero! this should not happen!"
