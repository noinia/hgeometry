module Geometry.Triangle.Class
  (

  ) where

import Data.Util (Three, pattern Three)
import GHC.TypeLits
import Geometry.HalfSpace
import Geometry.Polygon.Class


class Polygon_ point 2 r => Triangle_ triangle d point r where
