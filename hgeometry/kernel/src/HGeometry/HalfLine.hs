{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HalfLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Half-lines in \(d\)-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HalfLine
  ( HalfLine(..)
  , asOrientedLine
  , halfLineThrough
  , LineHalfLineIntersection(..)
  , HasDirection(..)
  ) where

import Control.Lens
import GHC.Generics (Generic)
import GHC.TypeLits
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Interval.Class
import HGeometry.Line.Intersection
import HGeometry.Line.Class
import HGeometry.Line.PointAndVector
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Text.Read
import Data.Type.Ord

--------------------------------------------------------------------------------

-- | A Halfline in R^d
data HalfLine point = HalfLine !point
                               !(Vector (Dimension point) (NumType point))

type instance Dimension (HalfLine point) = Dimension point
type instance NumType   (HalfLine point) = NumType point

deriving instance ( Eq point, Eq (Vector d r)
                  , d ~ Dimension point, r ~ NumType point) => Eq (HalfLine point)

instance ( Show point, Show r, d ~ Dimension point, r ~ NumType point
         , KnownNat d, Has_ Additive_ d r
         ) => Show (HalfLine point) where
  showsPrec k (HalfLine p v) = showParen (k > appPrec) $
                                 showString "HalfLine "
                               . showsPrec (appPrec+1) p
                               . showChar ' '
                               . showsPrec (appPrec+1) v

-- instance ( Show r, KnownNat d, Has_ Additive_ d r
--          ) => Show (HalfLine d r) where

appPrec :: Int
appPrec = 10

instance ( Read r, Read point, Has_ Additive_ d r, KnownNat d
         , d ~ Dimension point, r ~ NumType point
         ) => Read (HalfLine point) where
  readPrec = parens (prec appPrec $ do
                        Ident "HalfLine" <- lexP
                        p <- step readPrec
                        v <- step readPrec
                        pure (HalfLine p v))

instance HasStart (HalfLine point) point where
  start = lens (\(HalfLine p _) -> p) (\(HalfLine _ v) p -> HalfLine p v)
  {-# INLINE start #-}

instance HasDirection (HalfLine point) where
  direction = lens (\(HalfLine _ v) -> v) (\(HalfLine p _) v -> HalfLine p v)
  {-# INLINE direction #-}

instance Point_ point d r => HasSupportingLine (HalfLine point) where
  supportingLine (HalfLine p v) = LinePV (p^.asPoint) v
  {-# INLINE supportingLine #-}

--------------------------------------------------------------------------------

instance ( Point_ point d r, Ord r, Num r
         , HasOnLine (LinePV d r) d
         , Has_ Metric_ d r
         , Has_ Metric_ (d+1) r, Has_ Vector_ (1+d) r
         , d < d+1 -- TODO: this constraint is silly
         ) => Point d r `HasIntersectionWith` HalfLine point where
  q `intersects` hl = q `onLine` l && q `intersects` HalfSpace Positive h
    where
      l@(LinePV p v) = asOrientedLine hl
      h              = fromPointAndNormal p v :: HyperPlane d r


instance (Ord r, Num r, Point_ point 2 r
         ) => HasIntersectionWith (LinePV 2 r) (HalfLine point) where
  l@(LinePV _ u) `intersects` (HalfLine q w) = case q `onSide` l of
      OnLine    -> True
      LeftSide  -> (q .+^ w) `onSide` l' == RightSide
      RightSide -> (q .+^ w) `onSide` l' == LeftSide
    where
      l' = LinePV (q^.asPoint) u
    -- we construct a line l' parallel to l that goes through the startPoint of our
    -- ray/halfline.
    --
    -- Now if the start point was left of l, going in the direction of the ray we must end
    -- up right of l'. Symmetrically, if the starting point was right of the ray, we must
    -- go left to intersect instead.

type instance Intersection (LinePV 2 r) (HalfLine point) =
  Maybe (LineHalfLineIntersection (Point 2 r) (HalfLine point))

-- | Data type representing the intersection of a Line and a HalfLine
data LineHalfLineIntersection point halfLine =
      Line_x_HalfLine_Point    point
    | Line_x_HalfLine_HalfLine halfLine
  deriving (Show,Eq,Read,Ord,Generic,Functor)


instance ( Ord r, Fractional r, Point_ point 2 r
         ) => IsIntersectableWith (LinePV 2 r) (HalfLine point) where
  l `intersect` hl = m `intersect` l >>= \case
      Line_x_Line_Point p
        | p `onSide` perpendicularTo m == LeftSide -> Just $ Line_x_HalfLine_Point p
        | otherwise                                -> Nothing
      Line_x_Line_Line _                           -> Just $ Line_x_HalfLine_HalfLine hl
    where
      m = supportingLine hl
    -- the left side is suposedly the halfplane containing the halfLine

--------------------------------------------------------------------------------

instance ( Point_ point d r
         , Has_ Metric_ d r
         , Ord r, Fractional r
         , MkHyperPlaneConstraints d r
         ) => HasSquaredEuclideanDistance (HalfLine point) where
  pointClosestTo q hl@(HalfLine p v)
      | r `intersects` h = r
      | otherwise        = p'
    where
      p' = p^.asPoint
      r  = pointClosestTo q (supportingLine hl)
      h  = HalfSpace Positive (fromPointAndNormal p' v) :: HalfSpace d r
  -- main idea: compute the point closest to the supporting line of the halfline.  if this
  -- point lies in the halfspace h defined by the ray (e.g. for which the ray is the
  -- normal), then we've actually found the point closest to the ray. Otherwise the origin
  -- of the ray is simply the closest point.



--------------------------------------------------------------------------------

-- | Given two points p and q, create a halfline from p through q.
--
-- >>> halfLineThrough (Point2 5 10) (Point2 10 30 :: Point 2 Int)
-- HalfLine (Point2 5 10) (Vector2 5 20)
halfLineThrough     :: (Point_ point d r, Num r) => point -> point -> HalfLine point
halfLineThrough p q = HalfLine p (q .-. p)


-- | Convert the Halfline into an oriented line.
asOrientedLine                :: (Point_ point d r) => HalfLine point -> LinePV d r
asOrientedLine (HalfLine p v) = LinePV (p^.asPoint) v
