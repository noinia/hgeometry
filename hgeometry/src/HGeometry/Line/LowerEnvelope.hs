{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.LowerEnvelope
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Compute the lower envelope of a set of lines. The particular algorithm used here
-- dualizes the lines to points, computes the upper hull, and dualizes everything back.
--
--------------------------------------------------------------------------------
module HGeometry.Line.LowerEnvelope
  ( LowerEnvelopeF(..)
  , asAlternating

  , lowerEnvelope
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
import qualified HGeometry.ConvexHull.GrahamScan as CH
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
import           HGeometry.Sequence.Alternating
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | The lower envelope of a set of lines
newtype LowerEnvelopeF f vertex line = LowerEnvelope (Alternating f vertex line)

asAlternating                     :: LowerEnvelopeF f vertex line -> Alternating f vertex line
asAlternating (LowerEnvelope alt) = alt

deriving instance ( Show line, Show (f (vertex, line))
                  ) => Show (LowerEnvelopeF f vertex line)

deriving instance ( Eq line, Eq (f (vertex, line))
                  ) => Eq (LowerEnvelopeF f vertex line)

deriving instance ( Ord line, Ord (f (vertex, line))
                  ) => Ord (LowerEnvelopeF f vertex line)

instance Functor f => Functor (LowerEnvelopeF f r) where
  fmap f = bimap id f

instance Functor f => Bifunctor (LowerEnvelopeF f) where
  bimap f g (LowerEnvelope alt) = LowerEnvelope $ bimap f g alt



--------------------------------------------------------------------------------

-- | Computes the lower envelope of a set of \(n\) lines.
--
-- pre: the input is a set (so no duplicates)
--
-- running time: \(O(n\log n)\)
lowerEnvelope    :: forall f line r.
                    ( NonVerticalHyperPlane_ line 2 r
                    , Fractional r, Ord r
                    , Foldable1 f, Functor f
                    , IsIntersectableWith line line
                    , Intersection line line ~ Maybe (LineLineIntersection line)

                    , Default line -- TODO hack
                    )
                 => f line -> LowerEnvelopeF [] (Point 2 r) line
lowerEnvelope = construct . fmap (view extra)
              . CH.upperHull'
              . fmap (\l -> dualPoint l :+ l)
  where
    construct (l0 :| ls) = LowerEnvelope $ Alternating l0 $ computeVertices l0 ls

    computeVertices    :: line -> [line] -> [(Point 2 r, line)]
    computeVertices l0 = snd . List.mapAccumL (\l l' -> (l', (pt $ l `intersect` l', l'))) l0
    -- we scan through the lines from left to right: we maintain the previous line l, as
    -- we encounter an new line l' we report a pair (p,l') where p is the intersection
    -- point p of l and l'. Our last line l' becomes the new state.
    pt :: Maybe (LineLineIntersection line) -> Point 2 r
    pt = \case
      Just (Line_x_Line_Point p) -> p
      _                          -> error "lowerEnvelope: absurd: lines don't intersect"
