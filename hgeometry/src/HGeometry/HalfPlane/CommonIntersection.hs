{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection
  ( CommonIntersection(..)
  , LowerChain(..)
  , commonIntersection
  , lowerBoundary
  , LowerBoundary(..)
  ) where

import           Control.Lens
import           Data.Default.Class
import qualified Data.Foldable as F
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Vector
--------------------------------------------------------------------------------

-- | Common intersection of a bunch of halfplanes
data CommonIntersection halfPlane r =
    EmptyIntersection
  | Bounded (ConvexPolygon (Point 2 r :+ halfPlane))
  | Slab halfPlane () -- TODO needs a boundingLLine
    -- ^ each vertex stores the interior halfplane of the CCW-edge it is incident to.
  | Unbounded (LowerChain halfPlane r)
  deriving (Show,Eq)

-- | A polygonal chain bounding an unbounded convex region.
data LowerChain boundingLine r =
  LowerChain (Seq.Seq (boundingLine, Point 2 r)) -- ^ the bounded edges in left to right order
             boundingLine  -- ^ the unbounded halfplane defining the rightmost edge
  deriving (Show,Eq)

instance Functor (LowerChain boundingLine) where
  fmap f = bimap id f
instance Bifunctor LowerChain where
  bimap f g = bimap' f (over coordinates g)

-- | slightly more general version of bimap so we can easily flip the plane.
bimap'                       :: (boundingLine -> boundingLine')
                             -> (Point 2 r -> Point 2 s)
                             -> LowerChain boundingLine r -> LowerChain boundingLine' s
bimap' f g (LowerChain hs h) = LowerChain (fmap (bimap f g) hs) (f h)

--------------------------------------------------------------------------------

-- | Computes the common intersection of a \(n\) halfplanes.
--
-- running time: \(O(n\log n)\)
commonIntersection     :: ( Foldable1 f, Functor f
                          , HyperPlane_ halfPlane 2 r  -- this is not quite right yet
                          , Fractional r, Ord r
                          )
                       => f halfPlane -> CommonIntersection halfPlane r
commonIntersection hs0 = case NonEmpty.nonEmpty <$> foldMap partitionHalfPlanes hs0 of
  Vector2 Nothing       Nothing       -> error "commonIntersection: absurd, cannot happen"
  Vector2 Nothing       (Just aboves) -> Unbounded $ lowerBoundary aboves
  Vector2 (Just belows) Nothing       -> Unbounded $ upperBoundary belows
  Vector2 (Just belows) (Just aboves) -> let bb = lowerBoundary aboves
                                             ub = upperBoundary belows
                                         in undefined

-- | partitions the halfplanes into those with a negative sign (the first component of the
-- result) and positive signs.
partitionHalfPlanes :: (Foldable f, HalfSpace_ halfPlane d r
                       ) => f halfPlane -> Vector 2 [halfPlane]
partitionHalfPlanes = uncurry Vector2
                    . List.partition (\hl -> hl^.halfSpaceSign == Negative) . F.toList

--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all bounded from below,
-- computes their common intersection.
--
-- running time: O(n\log n)
lowerBoundary :: ( HalfPlane_ halfSpace r
                 , Foldable1 f, Fractional r, Ord r
                 )
              => f halfSpace -> LowerChain (BoundingHyperPlane halfSpace 2 r) r
lowerBoundary = initialize . dropParallel . sortOnCheap @Vector.Vector increasingSlope
                -- we sort lexicographically on increasing slope and decreasing intercept
  where
    initialize (h :| hs) = go (LowerChain mempty h) hs

    -- we go through the halfplanes by increasing slope. That means the newest bounding
    -- line is the steepest, and therefore is guaranteed to appear as the rightmost
    -- halfplane. We may have to drop some of the intermediate halfplanes though.
    go lowerChain = \case
      []      -> lowerChain
      (h':hs) -> dropFrom lowerChain h' `append` h'

    -- if there are parallel lines, the one with the highest intercept comes first. The
    -- corresponding haflplane is contained in the parallel halfplanes.
    dropParallel = map NonEmpty.head . NonEmpty.groupWith (^.slope)

    -- Appends the new bounding line at the end of the chain
    append (LowerChain hs h) h' = case toLineEQ h `intersect` toLineEQ h' of
      Just (Line_x_Line_Point p) -> LowerChain (hs Seq.|> (h,p)) h'
      _                          ->
        error "absurd: CommonIntersection, lower chain: parallel bounding lines!?"
      where
        toLineEQ = MkLineEQ . NonVerticalHyperPlane . view hyperPlaneCoefficients

-- | computes the slope of the bounding line of the halfplane; vertical lines are
-- represented first (in left to right order) ,and then lines are ordered
-- lexicographically on increasing slope. (Hence the slightly weird return type).
increasingSlope ::  ( HalfPlane_ halfSpace r, Fractional r, Ord r
                    ) => halfSpace -> Either r (Vector 2 r)
increasingSlope hl = undefined -- case hl^.boundingHyperPlane
  -- to dualPoint.vector of
  --                      Vector2


-- | Drop the edges whose left-endpoint lies below h'
dropFrom                        :: (HyperPlane_ boundingLine 2 r, Ord r, Num r)
                                => LowerChain boundingLine r -> boundingLine
                                -> LowerChain boundingLine r
dropFrom (LowerChain hs0 h0) h' = go h0 hs0
  where
    go h = \case
      Seq.Empty        -> LowerChain mempty h
      hs@(hs' :|> (m,p))
        | p `above` h' -> go m hs'    -- drop the last halfplane h, and continue
        | otherwise    -> LowerChain hs h

    q `above` h  = onSideTest q h /= LT

--------------------------------------------------------------------------------

upperBoundary = undefined

-- -- | Given the bounding lines of a bunch of halfplanes that are all bounded from above,
-- -- computes their common intersection.
-- --
-- -- running time: O(n\log n)
-- upperBoundary :: ( NonVerticalHyperPlane_ boundingLine 2 r
--                  , Fractional r, Ord r, Foldable f, Functor f
--                  )
--               => f boundingLine -> LowerBoundary (LowerChain boundingLine r)
-- upperBoundary = fmap (bimap' flipY (over yCoord negate)) . lowerBoundary . fmap flipY
--                 -- by flipping the plane, and using the existing lowerBoundary machinery
--   where
--     flipY = over (hyperPlaneCoefficients.traverse) negate
