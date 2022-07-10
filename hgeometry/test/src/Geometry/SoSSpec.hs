module Geometry.SoSSpec where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as DivideAndConquer
-- import qualified Geometry.SoS.ConvexHull as StrictConvexHull
import           Control.Lens
import           Data.Ext
import           Data.Indexed
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Data.RealNumber.Symbolic
import           Geometry.Point
import           Geometry.SoS.Orientation ()
import           Geometry.SoS.Point
import           Test.Hspec
import           Test.QuickCheck
import Geometry.Polygon.Convex (lowerTangent)

--------------------------------------------------------------------------------

type RBase = RealNumber 5
type R = Symbolic SoSI RBase

spec :: Spec
spec = pure ()

{-

lowerHullSymbolic :: (Ord r, Num r)
                  => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHullSymbolic = fmap (over core fromSymbolic)
                  . DivideAndConquer.lowerHull
                  . fmap (\(WithIndex i (p :+ e)) -> toSymbolic (WithIndex i p) :+ e)
                  . labelWithIndex


-- \(\prod_{0 \leq i \leq j} \varepsilon(i)^c > \varepsilon(k)\), for all \(1 \leq j < k\)


-- unfortunately, it seems that this is not really feasilbe to explicitly evaluate thee things.
spec :: Spec
spec = describe "symoblic/convex hull test" $ do
         it "convex hull same with symoblic" $ property $
           \(pts :: NonEmpty (Point 2 RBase :+ ())) ->
             lowerHullSymbolic pts `shouldBe` DivideAndConquer.lowerHull pts

         it "strict convex hull same with symoblic" $ property $
           \(pts :: NonEmpty (Point 2 RBase :+ ())) ->
             StrictConvexHull.lowerHull pts `shouldBe` DivideAndConquer.lowerHull pts

         it "toSymbolic/fromSymbolic" $ property $ \i (p :: Point 2 RBase) ->
           fromSymbolic (toSymbolic $ WithIndex i p) `shouldBe` p

         it "toSoSRational/fromSoSRational" $ property $ \i (p :: Point 2 RBase) ->
           fromSoSRational  (toSoSRational $ WithIndex i p) `shouldBe` p


-- spec :: Spec
-- spec = describe "SoS Symoblic" $ do
--          let eps' = (0.1 :: RealNumber 10)
--          it "comparing eps-folds " $ property $
--            \(ef1 :: EpsFold SmallInt) ef2 ->
--              let b = suitableBase ef1 `max` suitableBase ef2
--              in compare ef1 ef2 == compare (evalEps b eps' ef1) (evalEps b eps' ef2)

         -- forM_ [1..10] $ \(j :: Integer) ->
         --   forM_ [1..5] $ \c -> it "math" $
         --   let d = c+1
         --   in c * sum [ d^i | i <- [0..j]] < d ^ (j+1) `shouldBe` True

test :: NonEmpty (Point 2 RBase :+ ())
test = read "(Point2 (-4.65285) (-3.3007) :+ ()) :| [Point2 (-4.24464) 1.90593 :+ (),Point2 3.4645 (-2.71976) :+ ()]"

test' = StrictConvexHull.debug test

lh1 = NonEmpty.fromList $ NonEmpty.take 2 test'
rh1 = NonEmpty.fromList $ NonEmpty.drop 2 test'

foo = StrictConvexHull.lowerTangent (NonEmpty.reverse lh1) rh1

-}
