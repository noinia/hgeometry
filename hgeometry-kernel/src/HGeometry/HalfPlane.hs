module HGeometry.HalfPlane
  ( HalfPlane(..)
  ) where

import HGeometry.HalfPlane.Class
import HGeometry.Line
import HGeometry.Line.LineEQ
import HGeometry.Properties


--------------------------------------------------------------------------------

-- | Two dimensional Halfplane bounded by a line.
data GHalfPlane line r = LeftOf  !r
                       | RightOf !r
                       | Above !line
                       | Below !line
                       deriving (Show,Read,Eq)
type HalfPlane r = GHalfPlane (LineEQ r) r
type instance NumType (HalfPlane r) = r

-- type HalfPlane r = HalfSpace 2 r

-- class

--   HalfPlane_ halfPlane r | halfPlane -> r where



-- _LeftVerticalHalfplane :: Prism' (HalfPlane r) r
-- _LeftVerticalHalfplane = undefined
