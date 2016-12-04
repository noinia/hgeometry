{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QuickCheck.Instances where

import qualified Data.List.NonEmpty as NonEmpty
import Control.Lens
import Data.Ext
import Data.Range
import Data.Geometry
import Data.Geometry.Box
import Data.Semigroup
import Data.Geometry.SubLine
import Data.Geometry.Interval
import Data.Geometry.Vector
import Test.QuickCheck

instance Arbitrary a => Arbitrary (NonEmpty.NonEmpty a) where
  arbitrary = NonEmpty.fromList <$> listOf1 arbitrary


instance (Arbitrary r, Arity d) => Arbitrary (Vector d r) where
  arbitrary = vectorFromListUnsafe <$> infiniteList

instance (Arbitrary r, Arity d) => Arbitrary (Point d r) where
  arbitrary = Point <$> arbitrary

instance (Arbitrary r, Arity d, Num r) => Arbitrary (Line d r) where
  arbitrary = lineThrough <$> arbitrary <*> arbitrary

instance (Arbitrary r, Arity d, Ord r) => Arbitrary (Box d () r) where
  arbitrary = (\p (q :: Point d r) -> boundingBoxList' [p,q]) <$> arbitrary <*> arbitrary


instance Arbitrary r => Arbitrary (EndPoint r) where
  arbitrary = frequency [ (1, Open   <$> arbitrary)
                        , (9, Closed <$> arbitrary)
                        ]

instance (Arbitrary r, Ord r) => Arbitrary (Range r) where
  arbitrary = do
                l <- arbitrary
                r <- suchThat arbitrary (\x -> l^.unEndPoint <= x^.unEndPoint)
                return $ Range l r

instance (Arbitrary c, Arbitrary e) => Arbitrary (c :+ e) where
  arbitrary = (:+) <$> arbitrary <*> arbitrary

instance (Arbitrary r, Arbitrary p, Ord r, Ord p) => Arbitrary (Interval p r) where
  arbitrary = GInterval <$> arbitrary


instance (Arbitrary r, Arbitrary p, Arity d, Ord r, Ord p, Num r)
         => Arbitrary (SubLine d p r) where
  arbitrary = SubLine <$> arbitrary <*> arbitrary


instance (Arbitrary r, Arbitrary p, Arity d) => Arbitrary (LineSegment d p r) where
  arbitrary = LineSegment <$> arbitrary <*> arbitrary
