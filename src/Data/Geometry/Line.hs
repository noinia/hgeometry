{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Geometry.Line( module Data.Geometry.Line.Internal
                         ) where

import Data.Geometry.Line.Internal
import Data.Geometry.LineSegment
import Data.Geometry.Box
import Data.Geometry.Properties
import Data.Geometry.Point
import Data.Geometry.Boundary
import Data.Geometry.Transformation
import Data.Geometry.Vector

-- | Lines are transformable, via line segments
instance (Num r, AlwaysTruePFT d) => IsTransformable (Line d r) where
  transformBy t = supportingLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => Line d r -> LineSegment d () r
      toLineSegment' = toLineSegment


type instance IntersectionOf (Line 2 r) (Boundary (Rectangle p r)) =
  [ NoIntersection, Point 2 r, (Point 2 r, Point 2 r) , LineSegment 2 () r]


-- instance (Eq r, Fractional r)
--          => (Line 2 r) `IsIntersectableWith` (Boundary (Rectangle p r)) where
--   nonEmptyIntersection = defaultNonEmptyIntersection

--   _    `intersect` (Boundary Empty) = coRec NoIntersection
--   line `intersect` (Boundary rect)  = error "TODO"
--     where
--       (t,r,b,l) = sides' rect

--       ints = map (line `intersect`) [t,r,b,l]


type instance IntersectionOf (Line 2 r) (Rectangle p r) =
  [ NoIntersection, Point 2 r, LineSegment 2 () r]
