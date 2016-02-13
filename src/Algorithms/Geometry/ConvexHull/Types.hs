{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.ConvexHull.Types where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon



-- | Two dimensional convex hulls
newtype ConvexHull p r = ConvexHull { _extractHull :: (SimplePolygon p r) }
                       deriving (Show,Eq)
makeLenses ''ConvexHull
