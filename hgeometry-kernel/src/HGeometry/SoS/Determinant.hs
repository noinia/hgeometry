module HGeometry.SoS.Determinant where

import HGeometry.Number.Real.Symbolic
import HGeometry.Sign
import HGeometry.Matrix


-- | pre: computes the sign of the determinant
signDet   :: (HasDeterminant d, Ord i, Num r, Ord r) => Matrix d d (Symbolic i r) -> Sign
signDet m = case det m `compare` 0 of
              LT -> Negative
              GT -> Positive
              EQ -> error "signDet: determinant is zero! this should not happen!"
