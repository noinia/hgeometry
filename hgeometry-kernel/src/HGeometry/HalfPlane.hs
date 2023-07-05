module HGeometry.HalfPlane
  ( HalfPlane(..)
  ) where

import           HGeometry.Line
import           HGeometry.Line.LineEQ

--------------------------------------------------------------------------------

-- | Two dimensional Halfplane bounded by a line.
data HalfPlane r = LeftOf !r
                 | RightOf !r
                 | Above !(LineEQ r)
                 | Below !(LineEQ r)
                 deriving (Show,Read,Eq)
