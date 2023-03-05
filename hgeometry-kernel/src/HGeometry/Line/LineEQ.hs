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


  , evalAt'
  ) where

import Control.Subcategory.Foldable
import Control.Subcategory.Functor
import HGeometry.HyperPlane
import HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Line.Class
import HGeometry.Line.Intersection
import HGeometry.Point
import HGeometry.Properties(NumType, Dimension)
import HGeometry.Transformation
import HGeometry.Vector
import Text.Read

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
type instance VectorFamily d (LineEQ r) = WrapVector d (NonVerticalHyperPlane 2 r) (LineEQ r)

deriving instance Eq  (VectorFamily' 2 r) => Eq  (LineEQ r)
deriving instance Ord (VectorFamily' 2 r) => Ord (LineEQ r)

instance Constrained LineEQ where
  type Dom LineEQ r = OptCVector_ 2 r
instance CFunctor LineEQ where
  cmap f (LineEQ a b) = LineEQ (f a) (f b)
instance CTraversable LineEQ where
  ctraverse f (LineEQ a b) = LineEQ <$> f a <*> f b
instance CFoldable LineEQ where
  cfoldMap f (LineEQ a b) = f a <> f b


instance (Show r, OptCVector_ 2 r) => Show (LineEQ r) where
  showsPrec k (LineEQ a b) = showParen (k > appPrec) $
                              showString "LineEQ "
                            . showsPrec (appPrec+1) a
                            . showChar ' '
                            . showsPrec (appPrec+1) b

appPrec :: Int
appPrec = 10

instance (Read r, OptCVector_ 2 r) => Read (LineEQ r) where
  readPrec = parens (prec appPrec $ do
                          Ident "LineEQ" <- lexP
                          a <- step readPrec
                          b <- step readPrec
                          return (LineEQ a b))

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         , OptCVector_ 2 r
         , OptMetric_ 2 r
         ) => HyperPlane_ (LineEQ r) 2 r where

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         , OptCVector_ 2 r
         , OptMetric_ 2 r
         ) => ConstructableHyperPlane_ (LineEQ r) 2 r where

  -- | pre: the last component is not zero
  hyperPlaneFromEquation = MkLineEQ . hyperPlaneFromEquation

instance ( MkHyperPlaneConstraints 2 r
         , Fractional r
         , OptCVector_ 2 r
         , OptMetric_ 2 r
         ) => NonVerticalHyperPlane_ (LineEQ r) 2 r where
  evalAt (Point1_ x) = evalAt' x


instance ( OptCVector_ 2 r, OptMetric_ 2 r, OptCVector_ 3 r, Fractional r
         ) => Line_ (LineEQ r) 2 r where
  fromPointAndVec (Point2_ px py) (Vector2_ vx vy) =
    fromPointAndNormal (Point2 px py) (Vector2 (-vy) vx)

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
    = b' == 0 || a /= (a'/ (-b'))

instance (Eq r, Fractional r, OptCVector_ 2 r, OptCVector_ 3 r)
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
evalAt'                :: (Num r, OptCVector_ 2 r) => r -> LineEQ r -> r
evalAt' x (LineEQ a b) = a*x + b
-- TODO: it would be nice if this was actually just evalAt from the typeclass ....



-- evalAt''   :: ( Fractional r, OptCVector_ 2 r
--               , OptCVector_ 3 r, OptMetric_ 2 r
--               ) => r -> LineEQ r -> r
-- evalAt'' x = evalAt (Point1 x)


--------------------------------------------------------------------------------

-- | Lines are transformable, via line segments
instance ( Fractional r
         , TransformationConstraints 2 r
         , OptCVector_ 2 r, OptMetric_ 2 r, OptCVector_ 3 r
         ) => IsTransformable (LineEQ r) where
  -- | Warning, this may create vertical lines, which cannot be
  -- represented by this type. So be careful.
  transformBy t (LineEQ a b) = lineThrough p' q'
    where
      p' = transformBy t (Point2 0 b)
      q' = transformBy t (Point2 1 (a + b))
