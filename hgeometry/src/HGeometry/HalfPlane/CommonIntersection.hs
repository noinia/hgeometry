module HGeometry.HalfPlane.CommonIntersection
  (

  ) where

import           Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph


--------------------------------------------------------------------------------




-- data CommonIntersection

-- commonIntersection :: f halfPlane -> LowerEnvelope plane

-- commonIntersection' :: f (HalfPlane r) -> CommonIntersection

data LowerChain halfPlane =
  LowerChain (Seq.Seq (halfPlane, Point 2 (NumType halfPlane)))
               -- ^ the bounded edges in left to right order
             halfPlane  -- ^ the unbounded halfplane defining the rightmost edge

-- | Computes  the point dual to a line.
dualOf              :: LineEQ r -> Point 2 r
dualOf (LineEQ a b) = Point2 a (-b)

data LowerBoundary halfPlane = EntirePlane
                             | BoundedFromBelow (LowerChain halfPlane)
                             deriving (Show,Eq,Ord)

-- | Given a bunch of halfplanes that are all bounded from below,
-- computes their common intersection.
--
--
-- running time: O(n\log n)
lowerBoundary :: f halfPlane -> LowerBoundary halfPlane
lowerBoundary = initialize . dropParallel . sortOn dualOf
                -- we sort lexicographically on increasing slope and decreasing intercept
  where
    initialize = \case
      []          -> EntirePlane
      Just (h:hs) -> BoundedFromBelow $ go (LowerChain mempty h) hs

    go lowerChain = \case
      []      -> lowerChain
      (h':hs) -> dropFrom lowerChain h' `append` h'
        -- we go through the halfplanes by increasing slope. That
        -- means the newest halfplane has the steepest bounding line,
        -- and therefore is guaranteed to appear as the rightmost
        -- halfplane. We may have to drop some of the intermediate
        -- halfplanes though.

    dropParallel = map NonEmpty.head . NonEmpty.groupWith (^.slope)
                   -- if there are parallel lines, the one with the highest intercept
                   -- comes first. The corresponding haflplane is contained in the parallel
                   -- halfplanes.

dropFrom (LowerChain hs0 h0) h' = go h0 hs0
  where
    go h = \case
      EmptyR                            -> LowerChain empty h
      hs@(hs' :|> (m,p)) | p `below` h' -> go m hs' -- drop the last halfplane h, and continue
                         | otherwise    -> LowerChain hs1 h

append (LowerChain hs h) h' = case boundingLine h `intersect` boundingLine h' of
  Just (Line_x_Line_Point p) -> LowerChain (hs |> (h,p)) h'
  Nothing                    ->
    error "absurd: CommonIntersection, lower chain: parallel bounding lines!?"
