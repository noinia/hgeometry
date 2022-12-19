{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Kernel.Instances where

import HGeometry.Ball
import HGeometry.Interval
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Triangle
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Vector.Instances ()
import Test.QuickCheck

--------------------------------------------------------------------------------

instance Arbitrary v => Arbitrary (PointF v) where
  arbitrary = Point <$> arbitrary

instance Arbitrary r => Arbitrary (EndPoint ep r) where
  arbitrary = EndPoint <$> arbitrary

instance Arbitrary EndPointType where
  arbitrary = toEnum <$> arbitrary

instance Arbitrary r => Arbitrary (AnEndPoint r) where
  arbitrary = AnEndPoint <$> arbitrary <*> arbitrary


instance ( Arbitrary (endPoint r)
         , Ord (endPoint r)
         , OptCVector_ 2 (endPoint r)
         ) => Arbitrary (Interval endPoint r) where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (> p)
                 pure $ Interval p q

instance ( Arbitrary (endPoint point)
         , OptCVector_ 2 (endPoint point)
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
         , OptCVector_ 3 point
         , Eq point
         ) => Arbitrary (Triangle point) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary `suchThat` (/= a)
                 c <- arbitrary `suchThat` (\c' -> c' /= a && c' /= b)
                 pure $ Triangle a b c
    -- TODO: probably we don't awant to allow degenerate triangles?
