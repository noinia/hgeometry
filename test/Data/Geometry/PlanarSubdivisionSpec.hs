module Data.Geometry.PlanarSubdivisionSpec where

import           Data.Ext
import           Data.Foldable (toList, forM_)
import           Data.Geometry
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph (FaceId(..),VertexId(..))
import           Test.Hspec

data Test = Test
data Id a = Id a


triangle :: PlanarSubdivision Test () () PolygonFaceData Rational
triangle = (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
         $ trianglePG

trianglePG = fromPoints . map ext $ [origin, Point2 10 0, Point2 10 10]


toNonEmpty = NonEmpty.fromList . toList

spec :: Spec
spec = do
  describe "PlanarSubdivision" $ do
    it "outerFaceId = 0 " $
      outerFaceId triangle `shouldBe` (FaceId $ VertexId 0)
    it "outerFace tests" $
      let [d] = toList $ holesOf (outerFaceId triangle) triangle
      in leftFace d triangle `shouldBe` (outerFaceId triangle)
    describe "incidentDarts" $ do
      forM_ (darts' triangle) $ \d ->
        it "incidentDarts indiv"  $
          boundary' d triangle `shouldBe` (toNonEmpty $ edges' triangle)
    -- this last test is nonsense
