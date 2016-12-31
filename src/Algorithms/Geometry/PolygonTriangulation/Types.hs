module Algorithms.Geometry.PolygonTriangulation.Types where

import qualified Data.Foldable as F
import Data.Ext
import Data.CircularSeq (rotateL, rotateR, zip3LWith)
import Control.Lens
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Ord (comparing, Down(..))
import Data.Semigroup

import Debug.Trace

--------------------------------------------------------------------------------
