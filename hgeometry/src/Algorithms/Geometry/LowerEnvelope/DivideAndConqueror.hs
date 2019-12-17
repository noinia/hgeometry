module Algorithms.Geometry.LowerEnvelope.DivideAndConquereor where

import Algorithms.DivideAndConquer
import Data.Geometry.Point
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.List.NonEmpty(NonEmpty)
import Data.Vector

--------------------------------------------------------------------------------

type LowerEnvelope = GLowerEnvelope Vector

newtype GLowerEnvelope f = GLowerEnvelope (f (Segment r))

type Segment r = (Interval r, LineSegment 2 r)

-- Given a non-empty set of non-vertical lines, compute the lower envelope in
-- O(n log n) time.
lowerEnvelope :: NonEmpty (Line 2 r) -> LowerEnvelope r
lowerEnvelope = unM . divideAndConquer1 (Merge . single)

single :: Line 2 r -> LowerEnvelope r
single = undefined

newtype Merge r = Merge { unM :: LowerEnvelope r }

instance Semigroup (Merge r) where
  (Merge l) <> (Merge r) = undefined --merge' l r


-- merge' :: [LineSegment 2 r] -> [LineSegment 2 r] -> [LineSegment 2 r]
