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
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Point
import           HGeometry.Polygon.Convex

--------------------------------------------------------------------------------

-- | Common intersection of a bunch of halfplanes
data CommonIntersection halfPlane r =
    EmptyIntersection
  | Bounded (ConvexPolygon (Point 2 r :+ halfPlane))
    -- ^ each vertex stores the interior halfplane of the CCW-edge it is incident to.
  | Unbounded (LowerChain halfPlane r)
  deriving (Show,Eq)

data LowerChain boundingLine r =
  LowerChain (Seq.Seq (boundingLine, Point 2 r))
               -- ^ the bounded edges in left to right order
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
commonIntersection     :: ( Foldable f, Functor f
                          , HyperPlane_ halfPlane 2 r  -- this is not quite right yet
                          , Fractional r, Ord r
                          )
                       => f halfPlane -> CommonIntersection halfPlane r
commonIntersection hs0 = undefined
  where
    hs = partitionHalfPlanes hs0
    lb = fromMaybe . minimumByOf traverse (comparing (^.core)) $ lefts hs
      -- common intersection of the left halfplanes
    rb = fromMaybe . maximumByOf traverse (comparing (^.core)) $ rights hs
      -- common intersection of the right halfplanes
    fromMaybe = maybe EntirePlane BoundedBy

    bb = lowerBoundary $ aboves hs
    ub = upperBoundary $ belows hs







data HalfPlanes verticals nonVerticals = HalfPlanes { lefts  :: !verticals
                                                    , rights :: !verticals
                                                    , aboves :: !nonVerticals
                                                    , belows :: !nonVerticals
                                                    }
                                       deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)
instance (Semigroup verticals, Semigroup nonVerticals)
         => Semigroup (HalfPlanes verticals nonVerticals) where
  (HalfPlanes ls rs as bs) <> (HalfPlanes ls' rs' as' bs') =
    HalfPlanes (ls <> ls') (rs <> rs') (as <> as') (bs <> bs')
instance (Monoid verticals, Monoid nonVerticals)
         => Monoid (HalfPlanes verticals nonVerticals) where
  mempty = HalfPlanes mempty mempty mempty mempty


-- | Partition the halfplanes into left halfplanes (bounded from the
-- right), right halfplanes, aboves, and belows.
partitionHalfPlanes :: (Foldable f
                       )
                    => f halfPlane -> HalfPlanes [r :+ halfPlane] [LineEQ r :+ halfPlane]
partitionHalfPlanes = foldMap f mempty
  where
    f hl = undefined

--------------------------------------------------------------------------------

data LowerBoundary chain = EntirePlane
                         | BoundedBy chain
                         deriving (Show,Eq,Functor)

-- | Given the bounding lines of a bunch of halfplanes that are all
-- bounded from below, computes their common intersection.
--
--
-- running time: O(n\log n)
lowerBoundary :: ( Foldable f
                 , NonVerticalHyperPlane_ boundingLine 2 r
                 , Fractional r, Ord r
                 )
              => f boundingLine -> LowerBoundary (LowerChain boundingLine r)
lowerBoundary = initialize . dropParallel . sortOnCheap @V.Vector dualPoint
                -- we sort lexicographically on increasing slope and decreasing intercept
  where
    initialize = \case
      []   -> EntirePlane
      h:hs -> BoundedBy $ go (LowerChain mempty h) hs

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

-- | Appends the new bounding line at the end.
append                      :: (Fractional r, Ord r, NonVerticalHyperPlane_ boundingLine 2 r)
                            => LowerChain boundingLine r
                            -> boundingLine
                            -> LowerChain boundingLine r
append (LowerChain hs h) h' = case toLineEQ h `intersect` toLineEQ h' of
    Just (Line_x_Line_Point p) -> LowerChain (hs Seq.|> (h,p)) h'
    _                          ->
      error "absurd: CommonIntersection, lower chain: parallel bounding lines!?"
  where
    toLineEQ = MkLineEQ . NonVerticalHyperPlane . view hyperPlaneCoefficients

--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all
-- bounded from above, computes their common intersection.
--
-- running time: O(n\log n)
upperBoundary :: ( Foldable f, Functor f
                 , NonVerticalHyperPlane_ boundingLine 2 r
                 , Fractional r, Ord r
                 )
              => f boundingLine -> LowerBoundary (LowerChain boundingLine r)
upperBoundary = fmap (bimap' flipY (over yCoord negate)) . lowerBoundary . fmap flipY
                -- by flipping the plane
  where
    flipY = over (hyperPlaneCoefficients.traverse) negate
