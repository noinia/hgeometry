module Generate where

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Point.Random
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

genPoints   :: (Ord r, Fractional r, Arbitrary r) => Int -> IO (NonEmpty (Point 3 r :+ ()))
genPoints n = generate (NonEmpty.fromList . fmap (ext . unGP) <$> vectorOf n arbitrary)
