{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection.Bounded
  ( boundedCommonIntersection
  ) where

import           HGeometry.HalfPlane.CommonIntersection.Internal
import           HGeometry.Box (Rectangle, LineBoxIntersection(..))
import           HGeometry.Foldable.Sort
import           HGeometry.HalfSpace
import           HGeometry.Point
import           HGeometry.Triangle
import           HGeometry.Vector
import           HGeometry.Ext
import           HGeometry.Line
import           HGeometry.HyperPlane
import           HGeometry.Intersection
import           HGeometry.Polygon hiding (extremes)
import           HGeometry.HalfPlane.CommonIntersection.Chain as Chain
import           Control.Lens
import           Control.Monad ((=<<),(<=<))
import           Data.Foldable1
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Sequence.NonEmpty
import           Data.These
import           HGeometry.Sequence.Alternating
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           Prelude hiding (zipWith)
import           Data.Zip
import           Data.Coerce
import           Data.Foldable

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
                                , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                                , Intersection (BoundingHyperPlane halfPlane 2 r)
                                          (BoundingHyperPlane halfPlane 2 r)
                                  ~ Maybe (LineLineIntersectionG r (BoundingHyperPlane halfPlane 2 r))
                                , HasIntersectionWith (Point 2 r) halfPlane
                                , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r)
                                                     (BoundingHyperPlane halfPlane 2 r)


                                , Show r, Show halfPlane -- FIXME: remove
                                )
                              => set halfPlane
                              -> Maybe (ConvexPolygon (Point 2 r :+ halfPlane))
boundedCommonIntersection hs0 = case bimap extremes boundaries $ partitionHalfPlanes hs0 of
    This _verticals              -> Nothing
      -- by the precondition, if we only have vertical halfplanes they cannot form a bounded
      -- region, so then they must intersect in an empty region.
    That nonVerticals            -> case nonVerticals of
      BothSigns upper lower -> sweep ((^.extra) <$> lower) ((^.extra) <$> upper)
      _                     -> error "precondition failed; only non-verticals"

    These verticals nonVerticals -> case nonVerticals of
      These upper lower -> case verticals of
        Leftwards r    -> sweep ((^.extra) <$> lower)  (clipPushRight upper r)
        Rightwards l   -> sweep (clipPushLeft l lower) ((^.extra) <$> upper)
        LeftAndRights r l -> sweep (clipPushLeft l lower) (clipPushRight upper r)
      _                 -> Nothing
                         -- the non-verticals define an unbounded region;
                         -- so by the precondition that must mean the verticals must
                         -- define an empty intersection

  where
    clipPushLeft (x :+ h) chain = let chain' = clipLeft x chain
                                      y      = evalAt' x (chain'^.head1.core)
                                  in consChain (h,Point2 x y) ((^.extra) <$> chain')
    clipPushRight chain (x :+ h) = let chain' = clipRight x chain
                                       y      = evalAt' x (chain'^.last1.core)
                                   in snocChain ((^.extra) <$> chain') (Point2 x y, h)


-- | Computes the bounded intersection
--
sweep :: forall halfPlane r.
         (Intersection (BoundingHyperPlane halfPlane 2 r)
                                          (BoundingHyperPlane halfPlane 2 r)
                              ~ Maybe (LineLineIntersectionG r (BoundingHyperPlane halfPlane 2 r))
                            , Ord r, Fractional r
                            , HalfPlane_ halfPlane r
                            , HasIntersectionWith (Point 2 r) halfPlane
                            , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r)
                                                  (BoundingHyperPlane halfPlane 2 r)

                            )
      => Chain Seq r halfPlane
      -- ^ the lower boundary ; i.e. bounds the convex region from below
      -> Chain Seq r halfPlane
      -- ^ the upper boundary; i.e. bounds the region from above
      -> Maybe (ConvexPolygon (Point 2 r :+ halfPlane))
sweep lower upper =
    do (l,lower',upper')           <- findLeftmostIntersection lower upper
       (outputLower,outputUpper,r) <- findRightmostIntersection lower' upper'
       let vs = consSep l (coerce outputLower) <> consSep r (reversing . coerce $ outputUpper)

       -- pure . uncheckedFromCCWPoints . removeRepeated . fmap (uncurry (:+)) $ vs
       fromPoints (uncurry (:+) <$> vs)

       -- TODO: currently, the fromPoints still converts to an Vector, we are guaranteed
       -- to produce the points in CCW order. So I think we need to check only that the
       -- the first and last are distinct, and that the concatenation does not introduce
       -- duplicates.

-- -- | make sure that teh first and last point are distinct
-- noDup :: (Point_ point 2 r, Eq r) => ViewL1 point -> ViewL1 point
-- noDup z@(x :<< s) = case asViewR1 s of
--                       Just s'@(_ :>> y) | x^.asPoint == y^.asPoint -> viewl1 s'
--                                         | otherwise                -> z
--                       Nothing                                      -> z

--------------------------------------------------------------------------------

-- | Find the leftmost proper intersection between the two
-- chains. Returns the intersection point p and the remainder of the
-- chains (including the edges containing p)
--
-- \(O(n+m)\), where \(n\) and \(m\) are the length of the chains
--
-- If the chains intersect in a line segment this will return Nothing.
findLeftmostIntersection :: (Intersection (BoundingHyperPlane halfPlane 2 r)
                                          (BoundingHyperPlane halfPlane 2 r)
                              ~ Maybe (LineLineIntersectionG r (BoundingHyperPlane halfPlane 2 r))
                            , Ord r, Fractional r
                            , HalfPlane_ halfPlane r
                            , HasIntersectionWith (Point 2 r) halfPlane
                            , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r)
                                                  (BoundingHyperPlane halfPlane 2 r)
                            )
                         => Chain Seq r halfPlane
                        -- ^ the lower boundary ; i.e. bounds the convex region from below
                        -> Chain Seq r halfPlane
                        -- ^ the upper boundary; i.e. bounds the region from above
                        -> Maybe (Point 2 r, Chain Seq r halfPlane, Chain Seq r halfPlane)
findLeftmostIntersection lower upper = case (unconsChain lower, unconsChain upper) of
    (Left l,           Left u)           -> reportIntersection l u

    (Left l,           Right ((h,v),u'))
      | v `intersects` l    -> reportIntersection l h
      | otherwise           -> findLeftmostIntersection lower u' -- drop h; continue

    -- symmetric to the above
    (Right ((h',w),_), Left u)
      | w `intersects` u      -> reportIntersection h' u
      | otherwise             -> Nothing

    (Right ((h',w),l), Right ((h,v),u'))
      | w <= v    -> if w `intersects` h
                     then reportIntersection h' h
                          -- in general, it could be that v is not contained in h',
                          -- yet the bounding lines intersect. However, in that case their
                          -- common intersection is unbounded (i.e. as h' and h) diverge
                          -- towards the "top left".
                     else findLeftmostIntersection l upper -- drop h'
      | otherwise -> if v `intersects` h'
                     then reportIntersection h' h
                     else findLeftmostIntersection lower u' -- drop h
  where
    -- | Given the two leftmost halfplanes of the chain, test if they
    -- intersect in a point and if so report it and the full remaining
    -- chains. I.e. use this only when we are guarnteed that the first intersection
    -- lies on (the lines bounding these) halfplanes.
    reportIntersection l u = (l^.boundingHyperPlane) `intersect` (u^.boundingHyperPlane) >>= \case
      Line_x_Line_Point p     -> Just (p, lower, upper)
      Line_x_Line_Line _      -> Nothing -- the bounding halfplanes are the same.


--------------------------------------------------------------------------------

-- | Find the rightmost proper intersection between the two
-- chains. Returns the intersection point p and the remainder of the
-- chains (including the edges containing p)
--
-- \(O(n+m)\), where \(n\) and \(m\) are the length of the chains
--
-- If the chains intersect in a line segment this will return Nothing.
--
-- symmetric to findLeftmostIntersection
findRightmostIntersection             :: (Intersection (BoundingHyperPlane halfPlane 2 r)
                                                       (BoundingHyperPlane halfPlane 2 r)
                                          ~ Maybe (LineLineIntersectionG r (BoundingHyperPlane halfPlane 2 r))
                                         , Ord r, Fractional r
                                         , HalfPlane_ halfPlane r
                                         , HasIntersectionWith (Point 2 r) halfPlane
                                         , IsIntersectableWith (BoundingHyperPlane halfPlane 2 r)
                                                               (BoundingHyperPlane halfPlane 2 r)
                                         )
                                      => Chain Seq r halfPlane
                                      -- ^ the lower boundary ;
                                      -- i.e. bounds the convex region
                                      -- from below
                                      -> Chain Seq r halfPlane
                                      -- ^ the upper boundary;
                                      -- i.e. bounds the region from
                                      -- above
                                      -> Maybe ( Chain Seq r halfPlane
                                               , Chain Seq r halfPlane
                                               , Point 2 r
                                               )
findRightmostIntersection lower upper = case (unsnocChain lower, unsnocChain upper) of
    (Left l,           Left u)           -> reportIntersection l u

    (Left l,           Right (u',(v,h)))
      | v `intersects` l    -> reportIntersection l h
      | otherwise           -> findRightmostIntersection lower u' -- drop h; continue

    -- symmetric to the above
    (Right (_,(w,h')), Left u)
      | w `intersects` u      -> reportIntersection h' u
      | otherwise             -> Nothing

    (Right (l,(w,h')), Right (u',(v,h)))
      | w <= v    -> if w `intersects` h
                     then reportIntersection h' h
                          -- in general, it could be that v is not contained in h',
                          -- yet the bounding lines intersect. However, in that case their
                          -- common intersection is unbounded (i.e. as h' and h) diverge
                          -- towards the "top left".
                     else findRightmostIntersection l upper -- drop h'
      | otherwise -> if v `intersects` h'
                     then reportIntersection h' h
                     else findRightmostIntersection lower u' -- drop h
  where
    -- | Given the two rightmost halfplanes of the chain, test if they
    -- intersect in a point and if so report it and the full remaining
    -- chains. I.e. use this only when we are guarnteed that the first intersection
    -- lies on (the lines bounding these) halfplanes.
    reportIntersection l u = (l^.boundingHyperPlane) `intersect` (u^.boundingHyperPlane) >>= \case
      Line_x_Line_Point p     -> Just (lower, upper, p)
      Line_x_Line_Line _      -> Nothing -- the bounding halfplanes are the same.

--------------------------------------------------------------------------------

-- | Cons a separator onto the alternating
consSep                       :: sep -> Alternating Seq sep a -> ViewL1 (sep,a)
consSep s (Alternating x0 xs) = (s,x0) :<< xs
