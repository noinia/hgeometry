module Algorithms.Geometry.MapOverlay.MapOverlay where

import Data.Ext
import Data.Geometry.PlanarSubdivision
import Control.Lens
import Data.Geometry.Point
import Data.Geometry.LineSegment


data OverlayVtx r b e = Red r | Blue b | IntersectionVtx e deriving (Show,Eq)


mapOverlay      :: PlanarSubdivision s  v  e  f  r
                -> PlanarSubdivision s' v' e' f' r
                -> PlanarSubdivision s'' (OverlayVtx (VertexId' s :+ v) (VertexId' s' :+ v')
                                                     (Dart s :+ e,Dart s' :+ e'))
                                         (Either e e')
                                         (f,f')
                                         r
mapOverlay sr sb = undefined
