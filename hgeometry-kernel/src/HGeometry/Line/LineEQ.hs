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
  ( LineEQ(LineEQ, MkLineEQ)
  , slope, intercept


  , evalAt'
  ) where

import Control.Lens((^.), Lens', lens)
import HGeometry.HyperPlane
import HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Line.Class
import HGeometry.Line.Intersection
import HGeometry.Point
import HGeometry.Properties(NumType, Dimension)
-- import HGeometry.Transformation
import HGeometry.Vector
import Text.Read

--------------------------------------------------------------------------------

-- | A Line by its equation
newtype LineEQ r = MkLineEQ (NonVerticalHyperPlane 2 r)
  deriving newtype (Eq,Ord)

-- | Constructs a line in R^2, i.e. a line for the equation \(y = ax + b\)
pattern LineEQ     :: r -> r -> LineEQ r
pattern LineEQ a b = MkLineEQ (NonVerticalHyperPlane (Vector2 a b))
{-# COMPLETE LineEQ #-}

type instance NumType   (LineEQ r) = r
type instance Dimension (LineEQ r) = 2

-- | Lens to access the slope of a line
--
-- >>> (LineEQ 10 20) ^. slope
-- 10
slope :: Lens' (LineEQ r) r
slope = lens (\(LineEQ a _) -> a) (\(LineEQ _ b) a -> LineEQ a b)

-- | Lens to access the intercept (i.e. the value at which it
-- intersects the y-axis) of a line.
--
-- >>> (LineEQ 10 20) ^. slope
-- 20
intercept :: Lens' (LineEQ r) r
intercept = lens (\(LineEQ _ b) -> b) (\(LineEQ a _) b -> LineEQ a b)

-- deriving instance Eq  (VectorFamily' 2 r) => Eq  (LineEQ r)
-- deriving instance Ord (VectorFamily' 2 r) => Ord (LineEQ r)

-- instance Constrained LineEQ where
--   type Dom LineEQ r = OptCVector_ 2 r
-- instance CFunctor LineEQ where
--   cmap f (LineEQ a b) = LineEQ (f a) (f b)
-- instance CTraversable LineEQ where
--   ctraverse f (LineEQ a b) = LineEQ <$> f a <*> f b
-- instance CFoldable LineEQ where
--   cfoldMap f (LineEQ a b) = f a <> f b


instance (Show r) => Show (LineEQ r) where
  showsPrec k (LineEQ a b) = showParen (k > appPrec) $
                              showString "LineEQ "
                            . showsPrec (appPrec+1) a
                            . showChar ' '
                            . showsPrec (appPrec+1) b

appPrec :: Int
appPrec = 10

instance (Read r) => Read (LineEQ r) where
  readPrec = parens (prec appPrec $ do
                          Ident "LineEQ" <- lexP
                          a <- step readPrec
                          b <- step readPrec
                          return (LineEQ a b))

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         ) => HyperPlane_ (LineEQ r) 2 r where

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         ) => ConstructableHyperPlane_ (LineEQ r) 2 r where

  -- | pre: the last component is not zero
  hyperPlaneFromEquation = MkLineEQ
                         . hyperPlaneFromEquation @(NonVerticalHyperPlane 2 r)

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         ) => NonVerticalHyperPlane_ (LineEQ r) 2 r where
  evalAt p = evalAt' $ p^.xCoord


instance ( Fractional r
         ) => Line_ (LineEQ r) 2 r where
  fromPointAndVec p (Vector2 vx vy) =
    fromPointAndNormal p (Vector2 (-vy) vx)

----------------------------------------

-- | The intersection of two lines is either: NoIntersection, a point or a line.
type instance Intersection (LineEQ r) (LineEQ r) =
  Maybe (LineLineIntersection (LineEQ r))


instance (Eq r) => HasIntersectionWith (LineEQ r) (LineEQ r) where
  (LineEQ a _) `intersects` (LineEQ a' _) = a /= a'

instance (Eq r, Fractional r)
         => IsIntersectableWith (LineEQ r) (LineEQ r) where
  l@(LineEQ a b) `intersect` (LineEQ a' b')
    | a == a'   = if b == b' then Just (Line_x_Line_Line l) else Nothing
    | otherwise = let x = (b'-b) / (a-a')
                  in Just . Line_x_Line_Point $ Point2 x (evalAt' x l)

----------------------------------------

type instance Intersection (LineEQ r) (HyperPlane 2 r) =
  Maybe (LineLineIntersection (LineEQ r))

instance (Eq r, Fractional r
         ) => HasIntersectionWith (LineEQ r) (HyperPlane 2 r) where
  (LineEQ a _) `intersects` (HyperPlane (Vector3 _ a' b'))
    = b' == 0 || a /= (a'/ (-b'))

instance (Eq r, Fractional r)
         => IsIntersectableWith (LineEQ r) (HyperPlane 2 r) where
  l@(LineEQ a b) `intersect` (HyperPlane (Vector3 c' a' b'))
      | b' == 0   = point $ c'/ (-a')
      | a == d'   = if b == e' then Just (Line_x_Line_Line l) else Nothing
      | otherwise = point $ (e'-b) / (a-d')
    where
      d' = a'/(-b')
      e' = c'/(-b')

      point x = Just . Line_x_Line_Point $ Point2 x (evalAt' x l)




----------------------------------------



--------------------------------------------------------------------------------

-- | Evaluate the line at at given position.
evalAt'                :: Num r => r -> LineEQ r -> r
evalAt' x (LineEQ a b) = a*x + b
-- TODO: it would be nice if this was actually just evalAt from the typeclass ....

evalAt''   :: Fractional r => r -> LineEQ r -> r
evalAt'' p = evalAt (Point1 p)

-- evalAt''   :: ( Fractional r, OptCVector_ 2 r
--               , OptCVector_ 3 r, OptMetric_ 2 r
--               ) => r -> LineEQ r -> r
-- evalAt'' x = evalAt (Point1 x)


--------------------------------------------------------------------------------


-- -- | Lines are transformable, via line segments
-- instance ( Fractional r
--          , TransformationConstraints 2 r
--          , OptCVector_ 2 r, OptMetric_ 2 r, OptCVector_ 3 r
--          ) => IsTransformable (LineEQ r) where
--   -- | Warning, this may create vertical lines, which cannot be
--   -- represented by this type. So be careful.
--   transformBy t (LineEQ a b) = lineThrough p' q'
--     where
--       p' = transformBy t (Point2 0 b)
--       q' = transformBy t (Point2 1 (a + b))
