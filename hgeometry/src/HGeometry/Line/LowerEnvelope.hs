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
  , LowerEnvelope
  , _Alternating
  , lowerEnvelope

  , lineAt
  , lookupLEVertex
  ) where

import           Control.Lens
import           Data.Default.Class
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Type.Ord
import qualified Data.Vector as Vector
import           HGeometry.Algorithms.BinarySearch
import qualified HGeometry.ConvexHull.GrahamScan as CH
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating

--------------------------------------------------------------------------------

-- | The lower envelope of a set of lines
newtype LowerEnvelopeF f vertex line = LowerEnvelope (Alternating f vertex line)

-- | A lower envelope, where the data structure is a vector.
type LowerEnvelope = LowerEnvelopeF Vector.Vector

-- | projection function that turns a lower envelope into an alternating "list" of lines
-- and vertices.
_Alternating  :: Iso (LowerEnvelopeF f vertex line)  (LowerEnvelopeF f' vertex' line')
                     (Alternating f vertex line)     (Alternating f' vertex' line')
_Alternating = coerced

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


-- -- | change the functor type of the Lower envelope
-- mapF                       :: (f (vertex, line) -> g (vertex, line))
--                            -> LowerEnvelopeF f vertex line -> LowerEnvelopeF g vertex line
-- mapF f = _Alternating %~ Alternating.mapF f

-- (LowerEnvelope alt) = LowerEnvelope (Alternating.mapF f alt)

--------------------------------------------------------------------------------

-- | Computes the lower envelope of a set of \(n\) lines.
--
-- pre: the input is a set (so no duplicates)
--
-- running time: \(O(n\log n)\)
lowerEnvelope    :: forall g f line r.
                    ( NonVerticalHyperPlane_ line 2 r
                    , Fractional r, Ord r
                    , Foldable1 f, Functor f
                    , IsIntersectableWith line line
                    , Intersection line line ~ Maybe (LineLineIntersection line)
                    , HasFromFoldable g
                    , Default line -- TODO hack
                    )
                 => f line -> LowerEnvelopeF g (Point 2 r) line
lowerEnvelope = construct . fmap (view extra) . NonEmpty.reverse
              . CH.upperHull'
              . fmap (\l -> dualPoint l :+ l)
  where
    construct (l0 :| ls) = let vertices' = computeVertices l0 ls
                           in LowerEnvelope $ Alternating l0 $ fromList vertices'

    computeVertices    :: line -> [line] -> [(Point 2 r, line)]
    computeVertices l0 = snd . List.mapAccumL (\l l' -> (l', (pt $ l `intersect` l', l'))) l0
    -- we scan through the lines from left to right: we maintain the previous line l, as
    -- we encounter an new line l' we report a pair (p,l') where p is the intersection
    -- point p of l and l'. Our last line l' becomes the new state.
    pt :: Maybe (LineLineIntersection line) -> Point 2 r
    pt = \case
      Just (Line_x_Line_Point p) -> p
      _                          -> error "lowerEnvelope: absurd: lines don't intersect"

--------------------------------------------------------------------------------

-- | Commputes the line directly above the query point x
lineAt                                          :: ( Ord r
                                                   , Point_ vertex d r
                                                   , 1 <= d
                                                   , BinarySearch (f (vertex, line))
                                                   , Elem (f (vertex, line)) ~ (vertex, line)
                                                   ) => r -> LowerEnvelopeF f vertex line -> line
lineAt x env@(LowerEnvelope (Alternating l0 _)) = case lookupLEVertex x env of
    Nothing     -> l0
    Just (_, l) -> l

-- | returns the rightmost vertex v for which v_x < x.
lookupLEVertex                                      :: ( Ord r
                                                       , Point_ vertex d r
                                                       , 1 <= d
                                                       , BinarySearch (f (vertex, line))
                                                       , Elem (f (vertex, line)) ~ (vertex, line)
                                                       )
                                                    => r -> LowerEnvelopeF f vertex line
                                                    -> Maybe (vertex, line)
lookupLEVertex x (LowerEnvelope (Alternating _ ls)) =
  binarySearchLastIn (not . \(v,_) -> v^.xCoord <= x) ls
