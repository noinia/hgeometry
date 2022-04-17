module Algorithms.Geometry.SSSPSpec (spec) where

import           Algorithms.Geometry.SSSP
import qualified Algorithms.Geometry.SSSP.Naive as Naive
import           Geometry
import           Geometry.PolygonSpec      ()
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances      ()

-- p1 :: SimplePolygon () Rational
-- p1 = fromPoints $ map ext [Point2 0 0, Point2 1 0, Point2 1 1, Point2 0 1]

-- p5 :: SimplePolygon () Rational
-- p5 = fromPoints $ map ext
--   [Point2 4312521495778239 58119566291039296
--   ,Point2 3732371122103376 57810205118554752
--   ,Point2 3732325702472896 57810180898851072
--   ,Point2 4556597042600896 57721801696080256
--   ,Point2 5600216371172672 57955724398345088
--   ,Point2 6312893694898688 57462889614230272
--   ,Point2 6931780844255872 57199904758059072
--   ,Point2 6742629568279488 58379729296430848]

spec :: Spec
spec = do
  specify "Fast.sssp == Naive.sssp" $
    property $ \(poly :: SimplePolygon () Rational) ->
      size poly < 50 ==>
      sssp (triangulate poly) === Naive.sssp poly
