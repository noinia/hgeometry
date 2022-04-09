module Data.Geometry.Point.Random where

import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------
-- * Some attempt at producing point sets more or less in general position

newtype GeneralPos point = GeneralPos { unGP :: point }
  deriving newtype (Show,Eq,Ord)

instance (Fractional r, Arity d) => Arbitrary (GeneralPos (Point d r)) where
  arbitrary = GeneralPos
            . fmap (\x -> myUpper * (fromIntegral x / fromIntegral (maxBound @Word64)))
              <$> choose @(Point d Word64) (pure 0,maxBound)
    where
      myUpper = 100

-- testPoints = sample
