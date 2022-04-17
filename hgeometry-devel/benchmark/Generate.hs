module Generate where

import           Data.Ext
import           Geometry.Point
import           Geometry.Point.Random
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

genPoints   :: (Ord r, Fractional r, Arbitrary r) => Int -> IO (NonEmpty (Point 3 r :+ Int))
genPoints n = generate (NonEmpty.fromList . withIndices . fmap unDelaunay <$> vectorOf n arbitrary)
  where
    withIndices xs = zipWith (:+) xs [0..]
