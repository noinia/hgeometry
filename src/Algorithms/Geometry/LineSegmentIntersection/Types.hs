{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.LineSegmentIntersection.Types where

import           Control.Lens
import Data.Geometry.Point
import Data.Geometry.LineSegment

data IntersectionPoint p r =
  IntersectionPoint { _intersectionPoint :: Point 2 r
                    , _endPointOf        :: [LineSegment 2 p r]
                    , _interiorTo        :: [LineSegment 2 p r]
                    } deriving (Show,Eq)

makeLenses ''IntersectionPoint

-- newtype E a b = E (a -> b)
