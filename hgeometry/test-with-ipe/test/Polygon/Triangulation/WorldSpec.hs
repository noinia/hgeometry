{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.WorldSpec (spec, Country) where

import           Control.Lens hiding (elements)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Golden
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances (shrinkPolygon)
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import           Ipe
import           System.IO.Unsafe
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

-- import           Test.QuickCheck.Instances ()
-- import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5

worldPath :: OsPath
worldPath = [osp|test-with-ipe/Polygon/Triangulation/world.ipe|]

{-# NOINLINE allCountries #-}
allCountries :: [Country]
allCountries = unsafePerformIO $
                do worldFile <- getDataFileName worldPath
                   polies <- readAllFrom @(SimplePolygon (Point 2 R)) worldFile
                   let polies' = filter (hasNoSelfIntersections . (^.core)) polies
                   return $ (Country . (^.core)) <$> polies'


newtype Country = Country (SimplePolygon (Point 2 R))
             deriving (Show,Eq)



instance Arbitrary Country where
  arbitrary = elements allCountries
  shrink (Country pg) = Country <$> shrinkPolygon pg


toPG :: SimplePolygon (Point 2 R) -> SimplePolygon (Point 2 R)
toPG = fromJust . fromPoints . toNonEmptyOf outerBoundary

spec :: Spec
spec = describe "triangulate World" $ do
         prop "all polygons are CCW" $
           \(Country pg) -> isCounterClockwise pg
         prop "all polygons are CCW after toPolygoning" $
           \(Country pg) -> isCounterClockwise $ toPG pg
         prop "triangulate yields only triangles" $
           \(Country pg) -> let subdivision = triangulate pg
                                triangs = subdivision^..interiorFacePolygons
                            in all isTriangle' triangs

         describe "debugging myPoly" $ do
           let subdivision = triangulate myPoly
               triangs = subdivision^..interiorFacePolygons
               out = [iO' myPoly]
                 <> map (\triang ->
                                            iO' $ (triang&vertices %~ view core
                                                  :: SimplePolygon (Point 2 R)
                                                  )
                                         ) triangs
           goldenWith [osp|data/test-with-ipe/Polygon/Triangulation/|]
                 (ipeFileGolden { name = [osp|myPoly|] })
                 (addStyleSheet opacitiesStyle $ singlePageFromContent out)


         -- describe "debugging myPoly Prime" $ do
         --   let subdivision = triangulate myPoly'
         --       triangs = subdivision^..interiorFacePolygons
         --       out = [iO' myPoly']
                 -- <> map (\triang ->
                 --                            iO' $ (triang&vertices %~ view core
                 --                                  :: SimplePolygon (Point 2 R)
                 --                                  )
                 --                         ) triangs
           -- goldenWith [osp|data/test-with-ipe/Polygon/Triangulation/|]
           --       (ipeFileGolden { name = [osp|myPolyPrime|] })
           --       (addStyleSheet opacitiesStyle $ singlePageFromContent out)
           -- it "myPolyPrime is CCW" $
           --   isCounterClockwise myPoly' `shouldBe` True


isTriangle'    :: SimplePolygon_ simplePolygon point r => simplePolygon  -> Bool
isTriangle' pg = numVertices pg == 3

isCounterClockwise :: (Num r, Eq r, SimplePolygon_ simplePolygon point r)
                   => simplePolygon -> Bool
isCounterClockwise = (\x -> x == abs x) . signedArea2X



myPoly :: SimplePolygon (Point 2 R)
myPoly =
  uncheckedFromCCWPoints . NonEmpty.reverse . toNonEmptyOf outerBoundary $ myPoly'
myPoly' :: SimplePolygon (Point 2 R)
myPoly' =
  read "SimplePolygon [Point2 2996.84 650.6,Point2 2996.71 650.69,Point2 2996.3 650.94,Point2 2995.06 651.63,Point2 2991.84 652.53,Point2 2991.3 652.59,Point2 2990.06 652.67,Point2 2985.5 653.09,Point2 2984.43 653.31,Point2 2983.78 653.59,Point2 2982.88 654.08,Point2 2979.43 653.59,Point2 2977.97 653.64,Point2 2977.74 653.69,Point2 2977.61 653.77,Point2 2977.51 653.87,Point2 2977.65 654.42,Point2 2978.55 655.83,Point2 2978.66 655.93,Point2 2979.34 656.03,Point2 2979.57 656.06,Point2 2983.78 656.29,Point2 2983.98 656.23,Point2 2984.82 655.87,Point2 2985.03 655.81,Point2 2985.27 655.76,Point2 2987.46 655.35,Point2 2988.01 655.29,Point2 2988.3 655.28,Point2 2988.59 655.45,Point2 2988.76 655.51,Point2 2989.01 655.53,Point2 2989.32 655.51,Point2 2990.63 655.27,Point2 2990.84 655.22,Point2 2991.41 654.61,Point2 2991.54 654.39,Point2 2994.6 653.65,Point2 2995.12 653.5,Point2 2996.47 652.37,Point2 2996.53 652.26,Point2 2997.02 650.42]"



  -- read "SimplePolygon [Point2 669.413 292.72,Point2 671.295 292.83,Point2 673.202 292.86,Point2 675.121 292.82,Point2 675.769 292.77,Point2 676.429 292.7,Point2 677.09 292.59,Point2 678.244 292.33,Point2 679.089 292.05,Point2 682.884 290.37,Point2 687.742 288.11,Point2 688.087 287.82,Point2 688.13 287.68,Point2 688.106 287.55,Point2 688.019 287.4,Point2 687.544 287.25,Point2 686.927 287.16,Point2 685.729 287.03,Point2 684.125 286.97,Point2 682.829 286.95,Point2 681.527 286.96,Point2 679.57 286.99,Point2 677.59 287.11,Point2 663.89 287.98,Point2 662.501 288.13,Point2 651.985 290.45,Point2 651.51 290.61,Point2 651.294 290.74,Point2 651.14 290.88,Point2 651.176 291.04,Point2 651.275 291.18,Point2 651.51 291.33,Point2 651.8 291.46,Point2 652.417 291.61,Point2 653.096 291.72,Point2 653.596 291.78,Point2 655.552 291.9]"


  -- -- "SimplePolygon [Point2 1930.07 388.08,Point2 1929.98 387.91,Point2 1929.77 387.62,Point2 1929.58 387.51,Point2 1928.57 386.97,Point2 1927.95 386.76,Point2 1927.24 386.57,Point2 1926.75 386.47,Point2 1925.73 386.33,Point2 1925.06 386.28,Point2 1923.93 386.3,Point2 1923.53 386.35,Point2 1923.28 386.51,Point2 1923.3 386.67,Point2 1923.6 387.88,Point2 1923.89 387.99,Point2 1925.99 388.28,Point2 1926.58 388.33,Point2 1928.52 388.46,Point2 1929.04 388.43,Point2 1929.53 388.37]"
