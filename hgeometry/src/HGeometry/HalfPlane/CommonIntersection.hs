{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection
  ( CommonIntersection(..)
  , Chain(..)
  , commonIntersection
  , lowerBoundary
  -- , LowerBoundary(..)
  ) where

import           Control.Lens
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Sequence (Seq(..))
import           HGeometry.Ext
import           HGeometry.HalfSpace
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Sequence.Alternating
import           HGeometry.Vector
--------------------------------------------------------------------------------

-- | Common intersection of a bunch of halfplanes
data CommonIntersection halfPlane r =
    EmptyIntersection
  | Bounded (ConvexPolygon (Point 2 r :+ halfPlane))
  | Slab halfPlane () -- TODO needs a boundingLLine
    -- ^ each vertex stores the interior halfplane of the CCW-edge it is incident to.
  | Unbounded (Chain Seq halfPlane r)
  deriving (Show,Eq)

-- | A polygonal chain bounding an unbounded convex region, in CCW order.
newtype Chain f halfPlane r = Chain (Alternating f (Point 2 r) halfPlane)

deriving instance ( Show halfPlane, Show (f (Point 2 r, halfPlane))
                  ) => Show (Chain f halfPlane r)

deriving instance ( Eq halfPlane, Eq (f (Point 2 r, halfPlane))
                  ) => Eq (Chain f halfPlane r)

deriving instance ( Ord halfPlane, Ord (f (Point 2 r, halfPlane))
                  ) => Ord (Chain f halfPlane r)

instance Functor f => Functor (Chain f r) where
  fmap f = bimap id f
instance Functor f => Bifunctor (Chain f) where
  bimap f g (Chain alt) = Chain $ bimap (over coordinates g) f alt

-- -- | slightly more general version of bimap so we can easily flip the plane.
-- bimap'                       :: (boundingLine -> boundingLine')
--                              -> (Point 2 r -> Point 2 s)
--                              -> LowerChain boundingLine r -> LowerChain boundingLine' s
-- bimap' f g (LowerChain hs h) = LowerChain (fmap (bimap f g) hs) (f h)

--------------------------------------------------------------------------------

-- | Computes the common intersection of a \(n\) halfplanes.
--
-- running time: \(O(n\log n)\)
commonIntersection     :: ( Foldable1 f, Functor f
                          , HalfPlane_ halfPlane r
                          , Fractional r, Ord r
                          )
                       => f halfPlane -> CommonIntersection halfPlane r
commonIntersection hs0 = case NonEmpty.nonEmpty <$> foldr classifyHalfPlane (Vector2 [] []) hs0 of
    Vector2 Nothing       Nothing       -> error "commonIntersection: absurd, cannot happen"
    Vector2 Nothing       (Just aboves) -> Unbounded $ lowerBoundary aboves
    Vector2 (Just belows) Nothing       -> Unbounded $ upperBoundary belows
    Vector2 (Just belows) (Just aboves) -> let bb = lowerBoundary aboves
                                               ub = upperBoundary belows
                                           in undefined
  where
    classifyHalfPlane h (Vector2 belows aboves) = case h^.halfSpaceSign of
                                                    Negative -> Vector2 (h:belows) aboves
                                                    Positive -> Vector2 belows     (h:aboves)

--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all bounded from below,
-- computes their common intersection.
--
-- running time: O(n\log n)
lowerBoundary :: ( HalfPlane_ halfPlane r
                 , Foldable1 f, Fractional r, Ord r
                 )
              => f halfPlane -> Chain Seq halfPlane r
lowerBoundary = undefined


-- lowerBoundary = initialize . dropParallel . sortOnCheap @Vector.Vector increasingSlope
--                 -- we sort lexicographically on increasing slope and decreasing intercept
--   where
--     initialize (h :| hs) = go (LowerChain mempty h) hs

--     -- we go through the halfplanes by increasing slope. That means the newest bounding
--     -- line is the steepest, and therefore is guaranteed to appear as the rightmost
--     -- halfplane. We may have to drop some of the intermediate halfplanes though.
--     go lowerChain = \case
--       []      -> lowerChain
--       (h':hs) -> dropFrom lowerChain h' `append` h'

--     -- if there are parallel lines, the one with the highest intercept comes first. The
--     -- corresponding haflplane is contained in the parallel halfplanes.
--     dropParallel = map NonEmpty.head . NonEmpty.groupWith (^.slope)

--     -- Appends the new bounding line at the end of the chain
--     append (LowerChain hs h) h' = case toLineEQ h `intersect` toLineEQ h' of
--       Just (Line_x_Line_Point p) -> LowerChain (hs Seq.|> (h,p)) h'
--       _                          ->
--         error "absurd: CommonIntersection, lower chain: parallel bounding lines!?"
--       where
--         toLineEQ = MkLineEQ . NonVerticalHyperPlane . view hyperPlaneCoefficients

-- -- | computes the slope of the bounding line of the halfplane; vertical lines are
-- -- represented first (in left to right order) ,and then lines are ordered
-- -- lexicographically on increasing slope. (Hence the slightly weird return type).
-- increasingSlope ::  ( HalfPlane_ halfSpace r, Fractional r, Ord r
--                     ) => halfSpace -> Either r (Vector 2 r)
-- increasingSlope hl = undefined -- case hl^.boundingHyperPlane
--   -- to dualPoint.vector of
--   --                      Vector2


-- -- | Drop the edges whose left-endpoint lies below h'
-- dropFrom                        :: (HyperPlane_ boundingLine 2 r, Ord r, Num r)
--                                 => LowerChain boundingLine r -> boundingLine
--                                 -> LowerChain boundingLine r
-- dropFrom (LowerChain hs0 h0) h' = go h0 hs0
--   where
--     go h = \case
--       Seq.Empty        -> LowerChain mempty h
--       hs@(hs' :|> (m,p))
--         | p `above` h' -> go m hs'    -- drop the last halfplane h, and continue
--         | otherwise    -> LowerChain hs h

--     q `above` h  = onSideTest q h /= LT

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
