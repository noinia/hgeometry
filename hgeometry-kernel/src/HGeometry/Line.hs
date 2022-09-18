module HGeometry.Line
  ( module HGeometry.Line.Class
  , pattern Line
  ) where

import HGeometry.Line.Class
import HGeometry.HyperPlane.NonVertical
import HGeometry.Vector

--------------------------------------------------------------------------------


pattern Line     :: r -> r -> NonVerticalHyperPlane 2 r
pattern Line a b = NonVerticalHyperPlane (Vector2 a b)
