module HGeometry.HalfPlane.CommonIntersection.Internal where

import Data.These
import Control.Lens hiding (Empty)
import Data.Foldable1
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (comparing)
import Data.Sequence (Seq(..))
import Data.These
import HGeometry.Ext
import HGeometry.Foldable.Util
import HGeometry.HalfLine
import HGeometry.HalfPlane.CommonIntersection.Chain
import HGeometry.HalfSpace
import HGeometry.Polygon.Simple.Class
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Line.LowerEnvelope
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon.Convex
import HGeometry.Sequence.Alternating
import HGeometry.Vector
import HGeometry.Slab
import HGeometry.Properties

--------------------------------------------------------------------------------


-- | A these where both values just have the same type.  in particular, we will use This:
-- for Negative signs halfplanes, and That for Positive signed half planes.
type These2 a = These a a

-- Some pattern synonyms so that the rest is easier to read
pattern Negatives :: a -> These a b
pattern Negatives x = This x
pattern Positives :: b -> These a b
pattern Positives x = That x
pattern BothSigns :: a -> b -> These a b
pattern BothSigns x y = These x y
{-# COMPLETE Negatives, Positives, BothSigns #-}


pattern Leftwards :: a -> These a b
pattern Leftwards x = This x
pattern Rightwards :: b -> These a b
pattern Rightwards x = That x
pattern LeftAndRights :: a -> b -> These a b
pattern LeftAndRights x y = These x y
{-# COMPLETE Leftwards, Rightwards, LeftAndRights #-}


--------------------------------------------------------------------------------

-- | Vertical halfplanes; the left halfplanes (i.e. unbounded towards
-- the left) are in the This, the right halfplanes are in the That
type Verticals halfPlane r    = These2 (NonEmpty (r        :+ halfPlane))
-- | Non-vertical halfplanes. The halfplanes upward halfplanes are in the
-- This, the downward halfplanes are in the That.
type NonVerticals halfPlane r = These2 (NonEmpty (LineEQ r :+ halfPlane))

-- | Classify the halfplanes by their bounding lines into
-- Vertical/NonVertical, and then whether they are left/right of their
-- bounding lines or below/above them (in case of non-vertical).
--
-- Note that the sign of the halfplane may not necessarily agree with
-- the sign as given by the computed bounding line; e.g. if the
-- original input halfplane is the halfplane left of some oriented
-- line pointing "leftwards" (then its sign may be negative) whereas
-- we actually have a halfplane bounded from below.
partitionHalfPlanes     :: forall f halfPlane r.
                           ( Foldable1 f
                           , HalfPlane_ halfPlane r, Ord r, Fractional r
                           , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                           , HasIntersectionWith (Point 2 r) halfPlane
                           ) => f halfPlane
                        -> These (Verticals halfPlane r) (NonVerticals halfPlane r)
partitionHalfPlanes = bimap (partitionEithersNE . fmap classifyLR)
                            (partitionEithersNE . fmap classifyUD)
                    . partitionEithersNE . fmap classifyHalfPlane
                    . toNonEmpty
  where
    classifyLR half@(x :+ h)
      | Point2 (x-1) 0 `intersects` h = Left  half  -- h is a left halfplane
      | otherwise                     = Right half -- h is a right halfplane

    classifyUD half@(LineEQ _ b :+ h)
      | Point2 0 (b+1) `intersects` h = Right half -- h is an upward halfplane
      | otherwise                     = Left  half -- h is a downard halfpalne


-- | From all the left halfplanes we compute the the leftmost one,
-- and from the right halfplanes we compute the rightmost one.
extremes :: Ord r => Verticals halfPlane r -> These2 (r :+ halfPlane)
extremes = bimap leftMostPlane rightMostPlane
  where
    rightMostPlane = maximumBy (comparing (^.core))
    leftMostPlane  = minimumBy (comparing (^.core))

-- | Computes the upper boundary of the downward halfplanes, and the lower
-- boundary of the upward halfplanes.
--
-- Both boundaries are given in left-to-right order.
boundaries :: ( HalfPlane_ halfPlane r
              , Ord r, Fractional r
              ) => NonVerticals halfPlane r -> These2 (Chain Seq r (LineEQ r :+ halfPlane))
boundaries = bimap upperBoundary lowerBoundary
  where
    upperBoundary hs = let LowerEnvelope alt = lowerEnvelope hs in Chain alt
    lowerBoundary hs = let LowerEnvelope alt = upperEnvelope hs in Chain alt


-- | Classify the halfplane as either having a vertical bounding line or a general
-- non-vertical line.
classifyHalfPlane   :: ( HalfPlane_ halfPlane r
                       , HyperPlane_ (BoundingHyperPlane halfPlane 2 r) 2 r
                       , Fractional r, Eq r
                       )
                    => halfPlane
                    -> Either (r :+ halfPlane) (LineEQ r :+ halfPlane)
classifyHalfPlane h = case h^.boundingHyperPlane.to asGeneralLine of
  VerticalLineThrough x -> Left  (x :+ h)
  NonVertical l         -> Right (l :+ h)

-- | Convert to a general line.
asGeneralLine :: (HyperPlane_ hyperPlane 2 r, Fractional r, Eq r)
              => hyperPlane -> VerticalOrLineEQ r
asGeneralLine = hyperPlaneFromEquation . hyperPlaneEquation


--------------------------------------------------------------------------------
-- | We use the same type as the lower envelope
type UpperEnvelopeF = LowerEnvelopeF

-- | To compute the upper envelope we simply flip the plane, and compute the lower
-- envelope instead.
--
-- \(O(n\log n)\)
upperEnvelope :: forall g f line r.
                    ( NonVerticalHyperPlane_ line 2 r
                    , Fractional r, Ord r
                    , Foldable1 f, Functor f
                    , IsIntersectableWith line line
                    , Intersection line line ~ Maybe (LineLineIntersection line)
                    , HasFromFoldable g, Functor g
                    )
                 => f line -> UpperEnvelopeF g (Point 2 r) line
upperEnvelope = bimap (over yCoord negate) flipY . lowerEnvelope . fmap flipY
  where
    flipY :: line -> line
    flipY = over (hyperPlaneCoefficients.traverse) negate
