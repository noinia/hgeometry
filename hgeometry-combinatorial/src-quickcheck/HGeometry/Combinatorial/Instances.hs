{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Combinatorial.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the types in hgeometry-combinatorial
--
--------------------------------------------------------------------------------
module HGeometry.Combinatorial.Instances where

import           HGeometry.Ext
import           HGeometry.Cyclic
import           HGeometry.Number.Ratio.Generalized
import           HGeometry.Number.Real.Rational
import           HGeometry.Number.Real.Interval
import qualified HGeometry.Sign as Sign
-- import HGeometry.Number.Real.Symbolic
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           GHC.TypeLits
import           HGeometry.Tree.Binary.Static

import qualified Data.Set.NonEmpty as NESet

--------------------------------------------------------------------------------

instance (Arbitrary c, Arbitrary e) => Arbitrary (c :+ e) where
  arbitrary = (:+) <$> arbitrary <*> arbitrary
  shrink = genericShrink


--------------------------------------------------------------------------------

deriving newtype instance Arbitrary (f a) => Arbitrary (Cyclic f a)

--------------------------------------------------------------------------------

-- instance ( forall r. VectorLike_ (Vector d r)
--          ) => Arbitrary1 (Vector d) where
--   liftArbitrary gen = generateA (const gen)
--   -- I think this instance is unreachable, so let's comment it for now.

instance (Arbitrary a, Num a, Eq a) => Arbitrary (GRatio a) where
  arbitrary = (/) <$> arbitrary <*> (arbitrary `suchThat` (/= 0))
  shrink r = 0 : 1 : [ a' % b'
                     | a' <- shrink $ numerator r
                     , b' <- fromInteger 1 : shrink (denominator r)
                     , b' /= 0
                     ]

instance KnownNat p => Arbitrary (RealNumber p) where
  arbitrary = fromFixed <$> arbitrary
  shrink (RealNumber r) = RealNumber <$> shrink r

instance Arbitrary Sign.Sign where
  arbitrary = (\b -> if b then Sign.Positive else Sign.Negative) <$> arbitrary


instance (Arbitrary r, Real r, Fractional r) => Arbitrary (IntervalReal r) where
  arbitrary = realToFrac <$> arbitrary
  shrink x  = realToFrac <$> shrink (exactValue x)

--------------------------------------------------------------------------------
-- * Symbolic instances

-- instance (Arbitrary i, Ord i) => Arbitrary (EpsFold i) where
--   arbitrary = mkEpsFold . take 4 <$> listOf arbitrary

-- instance (Arbitrary r, Arbitrary (EpsFold i), Ord i) => Arbitrary (Term i r) where
  -- arbitrary = Term <$> arbitrary <*> arbitrary

-- instance (Arbitrary r, Ord i, Arbitrary (EpsFold i)) => Arbitrary (Symbolic i r) where
--   arbitrary = Sum <$> arbitrary

-- instance (Arbitrary a, Ord a) => Arbitrary (Bag a) where
--   arbitrary = foldMap singleton <$> listOf arbitrary

--------------------------------------------------------------------------------
-- * Binary tree instances

instance (Arbitrary a, Arbitrary v) => Arbitrary (BinLeafTree v a) where
  arbitrary = sized f
    where f n | n <= 0    = Leaf <$> arbitrary
              | otherwise = do
                              l <- choose (0,n-1)
                              Node <$> f l <*> arbitrary <*> f (n-l-1)
  -- shrink = genericShrink

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized f
    where f n | n <= 0    = pure Nil
              | otherwise = do
                              l <- choose (0,n-1)
                              Internal <$> f l <*> arbitrary <*> f (n-l-1)
  -- shrink = genericShrink


-- * Orphan instance
instance (Arbitrary a, Ord a) => Arbitrary (NESet.NESet a) where
  arbitrary = NESet.fromList <$> arbitrary
