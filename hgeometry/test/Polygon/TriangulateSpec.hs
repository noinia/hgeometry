{-# LANGUAGE OverloadedStrings #-}
module Polygon.TriangulateSpec (spec) where

import           Control.Lens
import           Data.Maybe (fromJust)
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import qualified HGeometry.Polygon.Triangulation.TriangulateMonotone as TM
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()



-- import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5



spec :: Spec
spec = do
  it "buggy polygon diagionals" $
    computeDiagonals buggyPolygon `shouldBe` [ Vector2 3 1
                                             , Vector2 3 0
                                             ]
  it "buggy polygon monotone area" $
    let g     = TM.triangulate @() buggyPolygon
        trigs = graphPolygons g
    in sum (map area trigs) `shouldBe` area buggyPolygon

  it "buggy polygon area" $
    let g     = triangulate @() buggyPolygon
        trigs = graphPolygons g
    in sum (map area trigs) `shouldBe` area buggyPolygon
  prop "sum (map area (triangulate polygon)) == area polygon" $
    \(poly :: SimplePolygon (Point 2 R)) ->
      let g = triangulate @() poly
          trigs = graphPolygons g
      in counterexample (show g) $ sum (map area trigs) === area poly
  prop "all isTriangle . triangulate" $
    \(poly :: SimplePolygon (Point 2 R)) ->
      let g = triangulate @() poly
          trigs = graphPolygons g
      in all isTriangle trigs

  it "buggy polygon2 " $
      let g = triangulate @() buggyPolygon2
          trigs = graphPolygons g
      in all isTriangle trigs `shouldBe` True

  it "buggy polygon2 not simplified " $
      let g = triangulate @() buggyNotSimplified
          trigs = graphPolygons g
      in all isTriangle trigs `shouldBe` True



  it "buggy polygon2 simplified " $
      let g = triangulate @() buggySimplified
          trigs = graphPolygons g
      in all isTriangle trigs `shouldBe` True

  -- prop "creating the graph does not create additional diagionals" $
  --   \poly ->
  --     let gr     = triangulate @() poly
  --         algSol = extractDiagonals gr
  --     in naiveSet algoSol `shouldBe` naiveSet (computeDiagionals poly)



buggyPolygon :: SimplePolygon (Point 2 R)
buggyPolygon = read "SimplePolygon [Point2 9 13,Point2 3 6,Point2 0 3,Point2 2 3,Point2 0 0]"

isTriangle :: SimplePolygon (Point 2 R) -> Bool
isTriangle = (== 3) . numVertices

graphPolygons    :: (Ord r, Num r, Point_ point 2 r)
                 => PlaneGraph s point PolygonEdgeType PolygonFaceData
                 -> [SimplePolygon (Point 2 r)]
graphPolygons gr = map (&vertices %~ view (core.asPoint)) $ gr^..interiorFacePolygons

buggyPolygon2 :: SimplePolygon (Point 2 R)
buggyPolygon2 = read "SimplePolygon [Point2 2589.22 470.87,Point2 2588.68 470.97,Point2 2587.88 471.15,Point2 2587.5 471.28,Point2 2586.96 471.57,Point2 2586.46 471.9,Point2 2586.4 472.02,Point2 2586.87 472.14,Point2 2587.24 472.17,Point2 2588.02 472.06,Point2 2588.79 471.9,Point2 2589.55 471.71,Point2 2590.7 471.38,Point2 2591.07 471.26,Point2 2591.17 471.12,Point2 2591.09 470.96,Point2 2590.87 470.84,Point2 2590.42 470.81,Point2 2589.98 470.8]"

buggySimplified :: SimplePolygon (Point 2 R)
buggySimplified = fromJust . fromPoints $
  [Point2 111.568 9.53226,
   Point2 92.1954 13.1198,
    Point2 63.4953 19.5773,
    Point2 49.8627 24.2411,
    Point2 30.4901 34.6449,
    Point2 12.5525 46.4837,
    Point2 10.4 50.7887,
    Point2 27.2613 55.0937,
    Point2 40.5352 56.17,
    Point2 68.5178 52.2237,
    Point2 96.1417 46.4837,
    Point2 123.407 39.6674,
    Point2 164.663 27.8286,
    Point2 177.937 23.5236,
    Point2 181.525 18.5011,
    Point2 178.655 12.761,
    Point2 170.762 8.456,
    Point2 154.618 7.37975]


buggyNotSimplified :: SimplePolygon (Point 2 R)
buggyNotSimplified = fromJust . fromPoints $
  [
    Point2 2589.22 470.87,
    Point2 2588.68 470.97,
    Point2 2587.88 471.15,
    Point2 2587.5  471.28,
    Point2 2586.96 471.57,
    Point2 2586.46 471.9 ,
    Point2 2586.4  472.02,
    Point2 2586.87 472.14,
    Point2 2587.24 472.17,
    Point2 2588.02 472.06,
    Point2 2588.79 471.9 ,
    Point2 2589.55 471.71,
    Point2 2590.7  471.38,
    Point2 2591.07 471.26,
    Point2 2591.17 471.12,
    Point2 2591.09 470.96,
    Point2 2590.87 470.84,
    Point2 2590.42 470.81,
    Point2 2589.98 470.8
    ]
