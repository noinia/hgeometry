--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.Minimalist
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Utilities for generating random points; in particular point sets
-- that are more or less in general position.
--
--------------------------------------------------------------------------------
module Data.Geometry.Point.Random where

import Data.Ext
import Data.Geometry.Ball
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------
-- * Uniform in some box


newtype GeneralPos point = GeneralPos { unGP :: point }
  deriving newtype (Show,Eq,Ord)

instance (Fractional r, Arity d) => Arbitrary (GeneralPos (Point d r)) where
  arbitrary = GeneralPos
            . fmap (\x -> myUpper * (fromIntegral x / fromIntegral (maxBound @Word64)))
              <$> choose @(Point d Word64) (pure 0,maxBound)
    where
      myUpper = 100

-- testPoints = sample


--------------------------------------------------------------------------------
-- * From some delaunay point set

newtype DelaunayP point = DelaunayP { unDelaunay :: point } deriving (Show,Eq,Ord)

lift              :: Num r => Point 2 r -> Point 3 r
lift (Point2 x y) = Point3 x y (x*x + y*y)

instance (Fractional r) => Arbitrary (DelaunayP (Point 3 r)) where
  arbitrary = (\(GeneralPos p) -> DelaunayP $ lift p) <$> arbitrary

--------------------------------------------------------------------------------
-- * Uniform in a ball

newtype UniformInBall point = UniformInBall { unUniformInBall :: point }
  deriving newtype (Show,Eq,Ord)

instance (Fractional r, Arity d, Ord r) => Arbitrary (UniformInBall (Point d r)) where
  arbitrary = fmap (UniformInBall . unGP) . suchThat arbitrary
              $ \(GeneralPos p) -> p `inClosedBall` myBall
    where
      myBall = Ball (ext . Point $ pure 50) (50*50)
