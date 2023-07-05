module HGeometry.HalfPlane
  ( HalfPlane(..)
  ) where

import HGeometry.HalfPlane.Class
import HGeometry.Line
import HGeometry.Line.LineEQ
import HGeometry.Properties


--------------------------------------------------------------------------------

-- | Two dimensional Halfplane bounded by a line.
data HalfPlane r = LeftOf !r
                 | RightOf !r
                 | Above !(LineEQ r)
                 | Below !(LineEQ r)
                 deriving (Show,Read,Eq)

type NumType (HalfPlane r) = r

-- type HalfPlane r = HalfSpace 2 r

-- class

--   HalfPlane_ halfPlane r | halfPlane -> r where



-- _LeftVerticalHalfplane :: Prism' (HalfPlane r) r
-- _LeftVerticalHalfplane = undefined
