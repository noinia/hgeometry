{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Combinatorial.Instances where

import Test.QuickCheck
import Test.QuickCheck.Instances ()


import Data.Ratio.Generalized
import Data.RealNumber.Symbolic

--------------------------------------------------------------------------------

instance (Arbitrary a, Num a, Eq a) => Arbitrary (GRatio a) where
  arbitrary = (%) <$> arbitrary <*> (arbitrary `suchThat` (/= 0))

instance (Arbitrary i, Ord i) => Arbitrary (EpsFold i) where
  arbitrary = mkEpsFold . take 4 <$> listOf arbitrary

instance (Arbitrary r, Arbitrary (EpsFold i), Ord i) => Arbitrary (Term i r) where
  arbitrary = Term <$> arbitrary <*> arbitrary

-- instance (Arbitrary r, Ord i, Arbitrary (EpsFold i)) => Arbitrary (Symbolic i r) where
--   arbitrary = Sum <$> arbitrary
