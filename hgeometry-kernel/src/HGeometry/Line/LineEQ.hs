{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line.LineEQ
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Non-vertical Lines in R^2 given by the equation \(y = ax + b\)
--
--------------------------------------------------------------------------------
module HGeometry.Line.LineEQ
  ( LineEQ
  , pattern LineEQ
  ) where

import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import HGeometry.HyperPlane.NonVertical
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Line.Intersection
import HGeometry.Point
import HGeometry.Properties(NumType, Dimension)
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A Line by its equation
newtype LineEQ r = MkLineEQ (NonVerticalHyperPlane 2 r)

-- | Constructs a line in R^2, i.e. a line for the equation \(y = ax + b\)
pattern LineEQ     :: OptCVector_ 2 r => r -> r -> LineEQ r
pattern LineEQ a b = MkLineEQ (NonVerticalHyperPlane (Vector2 a b))
{-# COMPLETE LineEQ #-}

type instance NumType   (LineEQ r) = r
type instance Dimension (LineEQ r) = 2
type instance VectorFor (LineEQ r) = Vector 2 r

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         , OptCVector_ 2 r
         , OptMetric_ 2 r
         ) => HyperPlane_ (LineEQ r) 2 r where

  -- | pre: the last component is not zero
  hyperPlaneFromEquation = MkLineEQ . hyperPlaneFromEquation

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         , OptCVector_ 2 r
         , OptMetric_ 2 r
         ) => NonVerticalHyperPlane_ (LineEQ r) 2 r where
  evalAt (Point1_ x) = evalAt' x

----------------------------------------

-- | The intersection of two lines is either: NoIntersection, a point or a line.
type instance Intersection (LineEQ r) (LineEQ r) =
  Maybe (LineLineIntersection (LineEQ r))


instance (Eq r, OptCVector_ 2 r) => HasIntersectionWith (LineEQ r) (LineEQ r) where
  (LineEQ a _) `intersects` (LineEQ a' _) = a /= a'

instance (Eq r, Fractional r, OptCVector_ 2 r)
         => IsIntersectableWith (LineEQ r) (LineEQ r) where
  l@(LineEQ a b) `intersect` (LineEQ a' b')
    | a == a'   = if b == b' then Just (Line_x_Line_Line l) else Nothing
    | otherwise = let x = (b'-b) / (a-a')
                  in Just . Line_x_Line_Point $ Point2 x (evalAt' x l)

----------------------------------------

type instance Intersection (LineEQ r) (HyperPlane 2 r) =
  Maybe (LineLineIntersection (LineEQ r))

instance (Eq r, Fractional r, OptCVector_ 2 r, OptCVector_ 3 r
         ) => HasIntersectionWith (LineEQ r) (HyperPlane 2 r) where
  (LineEQ a _) `intersects` (HyperPlane (Vector3 _ a' b'))
    = b' == 0 || a /= (a'/(negate b'))

instance (Eq r, Fractional r, OptCVector_ 2 r, OptCVector_ 3 r)
         => IsIntersectableWith (LineEQ r) (HyperPlane 2 r) where
  l@(LineEQ a b) `intersect` (HyperPlane (Vector3 c' a' b'))
      | b' == 0   = point $ (c'/(negate a'))
      | a == d'   = if b == e' then Just (Line_x_Line_Line l) else Nothing
      | otherwise = point $ (e'-b) / (a-d')
    where
      d' = a'/(negate b')
      e' = c'/(negate b')

      point x = Just . Line_x_Line_Point $ Point2 x (evalAt' x l)




----------------------------------------



--------------------------------------------------------------------------------

-- | Evaluate the line at at given position.
evalAt'                :: (Num r, OptCVector_ 2 r) => r -> LineEQ r -> r
evalAt' x (LineEQ a b) = a*x + b
-- TODO: it would be nice if this was actually just evalAt from the typeclass ....



-- evalAt''   :: ( Fractional r, OptCVector_ 2 r
--               , OptCVector_ 3 r, OptMetric_ 2 r
--               ) => r -> LineEQ r -> r
-- evalAt'' x = evalAt (Point1 x)
