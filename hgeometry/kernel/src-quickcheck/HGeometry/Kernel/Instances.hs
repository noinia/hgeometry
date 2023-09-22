{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Kernel.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the types in hgeometry-kernel
--
--------------------------------------------------------------------------------
module HGeometry.Kernel.Instances where

import Control.Lens hiding (cons)
import GHC.TypeLits
import HGeometry.Ball
import HGeometry.Box
import HGeometry.Combinatorial.Instances ()
import HGeometry.HalfSpace
import HGeometry.HyperPlane (HyperPlane(..))
import HGeometry.HyperPlane.NonVertical (NonVerticalHyperPlane(..))
import HGeometry.Interval
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
import HGeometry.Matrix
import HGeometry.Point
import HGeometry.Point.Instances ()
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.Vector
import HGeometry.Vector.Instances ()
import Test.QuickCheck

--------------------------------------------------------------------------------

-- instance Arbitrary v => Arbitrary (PointF v) where
--   arbitrary = Point <$> arbitrary

instance Arbitrary r => Arbitrary (EndPoint ep r) where
  arbitrary = EndPoint <$> arbitrary

instance Arbitrary EndPointType where
  arbitrary = (\b -> if b then Open else Closed) <$> arbitrary

instance Arbitrary r => Arbitrary (AnEndPoint r) where
  arbitrary = AnEndPoint <$> arbitrary <*> arbitrary


instance ( Arbitrary (endPoint r)
         , Ord (endPoint r)
         ) => Arbitrary (Interval endPoint r) where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (> p)
                 pure $ Interval p q

instance ( Arbitrary (endPoint point)
         , Eq (endPoint point)
         ) => Arbitrary (LineSegment endPoint point) where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (/= p)
                 pure $ LineSegment p q

instance ( Arbitrary point
         , Arbitrary (NumType point)
         , Ord (NumType point)
         , Num (NumType point)
         ) => Arbitrary (Ball point) where
  arbitrary = Ball <$> arbitrary
                   <*> (arbitrary `suchThat` (> 0))

instance ( Arbitrary point
         , Point_ point 2 r, Num r, Ord r
         , Eq point
         ) => Arbitrary (Triangle point) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary `suchThat` (/= a)
                 c <- arbitrary `suchThat` (\c' -> c' /= a && c' /= b && ccw a b c' /= CoLinear)
                 pure $ Triangle a b c

instance Arbitrary r => Arbitrary (LineEQ r) where
  arbitrary = LineEQ <$> arbitrary <*> arbitrary

instance ( Arbitrary r
         , Has_ Additive_ d r
         , Eq (Vector d r), Num r
         , KnownNat d
         ) => Arbitrary (LinePV d r) where
  arbitrary = LinePV <$> arbitrary
                     <*> (arbitrary `suchThat` (/= zero))


instance ( Arbitrary point
         , Arbitrary r
         , Point_ point d r
         , Num r
         , Ord (Vector d r)
         ) => Arbitrary (Box point) where
  arbitrary = (\p v -> Box p (p .+^ v)) <$> arbitrary
                                        <*> arbitrary `suchThat` (> zero)

instance ( Has_ Additive_ m r
         , Has_ Vector_ n (Vector m r)
         , Ixed (Vector n (Vector m r))
         , Ixed (Vector m r)
         , Arbitrary r
         ) =>
  Arbitrary (Matrix n m r) where
  arbitrary = (matrixFromRows :: Vector n (Vector m r) -> Matrix n m r)
           <$> arbitrary

instance ( Arbitrary r, Has_ Vector_ (d+1) r, Has_ Additive_ d r
         , Num r, Eq (Vector d r)) => Arbitrary (HyperPlane d r) where
  arbitrary = do a0                <- arbitrary
                 (a :: Vector d r) <- arbitrary `suchThat` (/= zero)
                 pure $ HyperPlane $ cons a0 a

instance (Arbitrary r, Has_ Additive_ d r
         , Num r, Eq (Vector d r)) => Arbitrary (NonVerticalHyperPlane d r) where
  arbitrary = NonVerticalHyperPlane <$> arbitrary `suchThat` (/= zero)


instance Arbitrary boundingHyperPlane => Arbitrary (HalfSpaceF boundingHyperPlane) where
  arbitrary = HalfSpace <$> arbitrary <*> arbitrary
