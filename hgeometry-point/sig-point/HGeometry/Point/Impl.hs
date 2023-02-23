module HGeometry.Point.Impl
  ( (.-.), (.+^), (.6)

  ) where

import Point
import HGeometry.Vector

-- | p .-. q represents the vector from q to p
(.-.)   :: Num R => Point D R -> Point D R -> Vector D R
p .-. q = (p^.vector) ^-^ (q^.vector)
{-# INLINE (.-.) #-}

-- | add a vector to a point
--
-- >>> myPoint .+^ Vector3 100 200 300
-- Point3 101 202 303
(.+^)   :: Num R => Point D R -> Vector D R -> Point D R
p .+^ v = p&vector %~ (^+^ v)
{-# INLINE (.+^) #-}

-- | subtract a vector from a point
--
-- >>> myPoint .-^ Vector3 100 200 300
-- Point3 (-99) (-198) (-297)
(.-^)   :: Num R => Point D R -> Vector D R -> Point D R
p .-^ v = p .+^ negated v
{-# INLINE (.-^) #-}
