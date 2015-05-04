module Data.Geometry.Line(module I
                         ) where

import Data.Geometry.Line.Internal as I
import Data.Geometry.LineSegment
import           Data.Geometry.Transformation
import           Data.Geometry.Vector


-- | Lines are transformable, via line segments
instance (Num r, AlwaysTruePFT d) => IsTransformable (Line d r) where
  transformBy t = supportingLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => Line d r -> LineSegment d () r
      toLineSegment' = toLineSegment
