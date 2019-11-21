module Data.Geometry.Polygon.Convex.LowerTangentSpec where


import           Algorithms.Geometry.ConvexHull.GrahamScan (convexHull, lowerHull)
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Lens
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry
import           Data.Geometry.Polygon (extremesLinear, fromPoints)
import           Data.Geometry.Polygon.Convex
import qualified Data.Geometry.Polygon.Convex.LowerTangent as LowerT
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Traversable (traverse)
import           Data.Util
import           Test.Hspec
import           Test.QuickCheck (Arbitrary(..), property, suchThat)
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = do it "LowerTangents the same" $
            property $ \lp rp -> let (lh,rh) = mkLowerTangentSets lp rp
                                 in lowerTangent lh rh == LowerT.lowerTangent lh rh

mkLowerTangentSets       :: (r ~ Word)
                         => NonEmpty (Point 2 r :+ ()) -> NonEmpty (Point 2 r :+ ())
                         -> (ConvexPolygon () r, ConvexPolygon () r)
mkLowerTangentSets lp rp = (lowerHull' lp, lowerHull' rp')
  where
    lowerHull' = ConvexPolygon . fromPoints . F.toList . lowerHull
    rp' = (\p -> p&core.xCoord %~ (+ w)) <$> rp
    w = maximum . fmap (^.core.xCoord) $ lp
