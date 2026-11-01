module Test.Common where

import Test.QuickCheck
import System.Random
import R
import Ipe.Color

--------------------------------------------------------------------------------

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

instance Arbitrary (IpeColor R) where
  arbitrary = Test.QuickCheck.elements basicNamedColors
