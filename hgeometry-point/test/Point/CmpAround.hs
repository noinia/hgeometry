module Point.CmpAround where

import Control.Lens ((^.))
import HGeometry.Ext
import HGeometry.Point


-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
ccwCmpAroundByQuadrant       :: (Num r, Ord r, Monoid p)
                             => Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p -> Ordering
ccwCmpAroundByQuadrant c q r = case (quadrantWith c q `compare` quadrantWith c r) of
                       EQ -> case ccw (c^.core) (q^.core) (r^.core) of
                         CCW      -> LT
                         CW       -> GT
                         CoLinear -> EQ
                       x -> x -- if the quadrant differs, use the order
                              -- specified by the quadrant.



cwCmpAroundByQuadrant :: (Num r, Ord r, Monoid p)
            => Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p -> Ordering
cwCmpAroundByQuadrant c q r = case (quadrantWith c q `compare` quadrantWith c r) of
                       EQ -> case ccw (c^.core) (q^.core) (r^.core) of
                         CCW      -> GT
                         CW       -> LT
                         CoLinear -> EQ
                       LT -> GT
                       GT -> LT -- if the quadrant differs, use the order
                                -- specified by the quadrant.

-- | Original implementation of cw with distance check
cwCmpAroundByQuadrantWithDist       :: (Num r, Ord r, Monoid p)
                                    => Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p
                                    -> Ordering
cwCmpAroundByQuadrantWithDist c q r = case (quadrantWith c q `compare` quadrantWith c r) of
                       EQ -> case ccw (c^.core) (q^.core) (r^.core) of
                         CCW      -> GT
                         CW       -> LT
                         CoLinear -> squaredEuclideanDist (c^.core) (q^.core)
                                     `compare`
                                     squaredEuclideanDist (c^.core) (r^.core)
                       LT -> GT
                       GT -> LT -- if the quadrant differs, use the order
                                -- specified by the quadrant.
