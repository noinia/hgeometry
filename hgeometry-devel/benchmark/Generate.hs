module Main where

import           Data.Aeson
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Point.Random
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           System.Environment (getArgs)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

type R = RealNumber 5

main :: IO ()
main = do [fp,n] <- getArgs
          pts <- genPoints @R (read n)
          encodeFile fp pts

genPoints   :: (Ord r, Fractional r, Arbitrary r) => Int -> IO (NonEmpty (Point 3 r :+ Int))
genPoints n = generate (NonEmpty.fromList . withIndices . fmap unGP <$> vectorOf n arbitrary)
  where
    withIndices xs = zipWith (:+) xs [0..]
