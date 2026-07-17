module HalfPlane.CommonIntersection.Bounded
  where

import           HGeometry.Foldable.Sort
import           HGeometry.HalfSpace
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Ext
import           HGeometry.HalfPlane.CommonIntersection.Chain as Chain
import           Control.Lens
import           Control.Monad (<=<)
import           Data.Foldable1
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------

-- | Given a (non)-empty set of n halfplanes with the guarantee that if
-- they have a non-empty common intersection X than this common intersection is
-- bounded. Compute the common intersection X.
--
-- pre: X is either empty or a non-empty two-dimensional set of points
--
-- this is for example useful if you want to compute the common intersection of halfplanes
-- restricted to soem bounded domain (triangle or box).
--
-- \(O(n\log n)\)
boundedCommonIntersection    :: forall set halfPlane r.
                                ( Foldable1 set
                                , HalfPlane_ halfPlane r
                                , Ord r, Fractional r
                                )
                             => set halfPlane -> Maybe (ConvexPolygon (Point 2 r :+ halfPlane))
boundedCommonIntersection hs = undefined
-- fmap (\h -> asLeftHalfPlane h :+ h) . toNonEmpty


-- | Given a (non)-empty set of n *left*halfplanes with the guarantee that if
-- they have a non-empty common intersection X than this common intersection is
-- bounded. Compute the common intersection X.
--
-- pre: X is either empty or a non-empty two-dimensional set of points
--
--
-- this is for example useful if you want to compute the common intersection of halfplanes
-- restricted to soem bounded domain (triangle or box).
--
-- \(O(n\log n)\)
boundedCommonIntersectionLeftHalfplanes    :: forall set r.
                                              ( Foldable1 set
                                              , HalfPlane_ halfPlane r
                                              , Ord r, Fractional r
                                              , BoundingHyperPlane halfPlane ~ LinePV 2 r
                                              )
                                           => set halfPlane
                                           -> Maybe (ConvexPolygon (Point 2 r :+ halfPlane))
boundedCommonIntersectionLeftHalfplanes = (toPolygon <=<)
                                        . foldrMap1 go Chain.singleton
                                        . NonEmptyVector.unsafeFromVector
                                        . sortBy (\h1 h2 ->
                                                    ccwCmpAround (origin @(Point 2 r))
                                                                 (Point $ h1^.boundingHyperPlane)
                                                                 (Point $ h2^.boundingHyperPlane)
                                                 )
    -- the Foldable1 constraint guarantees that the unsafeFromVector is safe.
  where
    go :: halfPlane -> Maybe (Chain Seq r halfPlane) -> Maybe (Chain Seq r halfPlane)
    go = undefined


    -- | Try to close the chain, turning it into a convex polygon
    toPolygon :: Chain Seq r halfPlane -> Maybe (ConvexPolygon (Point 2 r :+ halfPlane))
    toPolygon = undefined
