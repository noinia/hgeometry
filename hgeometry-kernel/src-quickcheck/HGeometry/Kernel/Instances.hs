{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Kernel.Instances where

import GHC.TypeLits
import HGeometry.Ball
import HGeometry.Box
import HGeometry.Interval
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
-- import HGeometry.Matrix
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.Vector
import HGeometry.Vector.Instances ()
import HGeometry.Point.Instances ()
import Test.QuickCheck
--------------------------------------------------------------------------------

-- instance Arbitrary v => Arbitrary (PointF v) where
--   arbitrary = Point <$> arbitrary

instance Arbitrary r => Arbitrary (EndPoint ep r) where
  arbitrary = EndPoint <$> arbitrary

instance Arbitrary EndPointType where
  arbitrary = toEnum <$> arbitrary

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
         , Eq point
         ) => Arbitrary (Triangle point) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary `suchThat` (/= a)
                 c <- arbitrary `suchThat` (\c' -> c' /= a && c' /= b)
                 pure $ Triangle a b c
    -- TODO: probably we don't awant to allow degenerate triangles?

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

-- instance ( OptVector_ m r
--          , OptVector_ n (Vector m r), KnownNat n, KnownNat m
--          , Arbitrary r, OptAdditive_ m r
--          ) =>

--   Arbitrary (Matrix n m r) where
--   arbitrary = (matrixFromRows :: Vector n (Vector m r) -> Matrix n m r)
--            <$> arbitrary
