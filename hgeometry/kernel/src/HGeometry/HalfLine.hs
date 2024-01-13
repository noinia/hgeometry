{-# LANGUAGE UndecidableInstances #-}
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
  ( HalfLine(MkHalfLine,HalfLine)


  , LineHalfLineIntersection(..)
  ) where

import Control.Lens
import Data.Coerce
import GHC.Generics (Generic)
import GHC.TypeLits
import HGeometry.Intersection
import HGeometry.Interval.Class
import HGeometry.Line.Intersection
import HGeometry.Line.PointAndVector
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Text.Read

--------------------------------------------------------------------------------

-- | A Halfline in R^d
newtype HalfLine d r = MkHalfLine (LinePV d r)

type instance Dimension (HalfLine d r) = d
type instance NumType   (HalfLine d r) = r

-- | Construct a Halfline from its starting point and the vector
pattern HalfLine     :: Point d r -> Vector d r -> HalfLine d r
pattern HalfLine p v = MkHalfLine (LinePV p v)
{-# COMPLETE HalfLine #-}

deriving newtype instance Eq (LinePV d r) => Eq (HalfLine d r)

instance ( Show r, KnownNat d, Has_ Additive_ d r
         ) => Show (HalfLine d r) where
  showsPrec k (HalfLine p v) = showParen (k > appPrec) $
                                 showString "HalfLine "
                               . showsPrec (appPrec+1) p
                               . showChar ' '
                               . showsPrec (appPrec+1) v

appPrec :: Int
appPrec = 10

instance (Read r, Has_ Additive_ d r, KnownNat d
         ) => Read (HalfLine d r) where
  readPrec = parens (prec appPrec $ do
                        Ident "HalfLine" <- lexP
                        p <- step readPrec
                        v <- step readPrec
                        pure (HalfLine p v))


_HalfLineLine :: Iso (HalfLine d r) (HalfLine d' r') (LinePV d r) (LinePV d' r')
_HalfLineLine = coerced


instance HasStart (HalfLine d r) (Point d r) where
  start = _HalfLineLine.anchorPoint
  {-# INLINE start #-}

instance HasDirection (HalfLine d r) where
  direction = _HalfLineLine.direction
  {-# INLINE direction #-}

instance HasSupportingLine (HalfLine d r) where
  supportingLine = coerce

--------------------------------------------------------------------------------


instance (Ord r, Num r) => HasIntersectionWith (LinePV 2 r) (HalfLine 2 r) where
  l@(LinePV _ u) `intersects` (HalfLine q w) = case q `onSide` l of
      OnLine    -> True
      LeftSide  -> (q .+^ w) `onSide` l' == RightSide
      RightSide -> (q .+^ w) `onSide` l' == LeftSide
    where
      l' = LinePV q u
    -- we construct a line l' parallel to l that goes through the startPoint of our
    -- ray/halfline.
    --
    -- Now if the start point was left of l, going in the direction of the ray we must end
    -- up right of l'. Symmetrically, if the starting point was right of the ray, we must
    -- go left to intersect instead.

type instance Intersection (LinePV 2 r) (HalfLine 2 r) =
  Maybe (LineHalfLineIntersection (Point 2 r) (HalfLine 2 r))

-- | Data type representing the intersection of a Line and a HalfLine
data LineHalfLineIntersection point halfLine =
      Line_x_HalfLine_Point    point
    | Line_x_HalfLine_HalfLine halfLine
  deriving (Show,Eq,Read,Ord,Generic,Functor)


instance (Ord r, Fractional r) => IsIntersectableWith (LinePV 2 r) (HalfLine 2 r) where
  l `intersect` hl = m `intersect` l >>= \case
      Line_x_Line_Point p
        | p `onSide` perpendicularTo m == LeftSide -> Just $ Line_x_HalfLine_Point p
        | otherwise                                -> Nothing
      Line_x_Line_Line _                           -> Just $ Line_x_HalfLine_HalfLine hl
    where
      m = supportingLine hl
    -- the left side is suposedly the halfplane containing the halfLine
