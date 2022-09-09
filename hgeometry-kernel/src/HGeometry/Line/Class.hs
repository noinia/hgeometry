module HGeometry.Line.Class
  ( Line_
  ) where

import HGeometry.HyperPlane.Class

type family Line_ line d r where
  Line_ line 2 r = HyperPlane_ line 2 r
