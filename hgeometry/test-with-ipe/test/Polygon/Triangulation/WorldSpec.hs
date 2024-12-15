{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.WorldSpec (spec, Country) where


import           Control.Lens hiding (elements)
import qualified Data.List.NonEmpty as NonEmpty
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
                   polies <- readAllFrom worldFile
                   let polies' = filter (hasNoSelfIntersections . (^.core)) polies
                   return $ (Country. (^.core)) <$> polies'


newtype Country = Country (SimplePolygon (Point 2 R))
             deriving (Show,Eq)



instance Arbitrary Country where
  arbitrary = elements allCountries
  shrink (Country pg) = Country <$> shrinkPolygon pg




spec :: Spec
spec = describe "triangulate World" $ do
         prop "triangulate yields only triangles" $
           \(Country pg) -> let subdivision = triangulate pg
                                triangs = subdivision^..interiorFacePolygons
                            in all isTriangle' triangs

         describe "debugging" $ do
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


isTriangle'    :: SimplePolygon_ simplePolygon point r => simplePolygon  -> Bool
isTriangle' pg = numVertices pg == 3


myPoly :: SimplePolygon (Point 2 R)
myPoly =
  uncheckedFromCCWPoints . NonEmpty.reverse . toNonEmptyOf outerBoundary $ myPoly'
myPoly' :: SimplePolygon (Point 2 R)
myPoly' =
  read "SimplePolygon [Point2 1930.07 388.08,Point2 1929.98 387.91,Point2 1929.77 387.62,Point2 1929.58 387.51,Point2 1928.57 386.97,Point2 1927.95 386.76,Point2 1927.24 386.57,Point2 1926.75 386.47,Point2 1925.73 386.33,Point2 1925.06 386.28,Point2 1923.93 386.3,Point2 1923.53 386.35,Point2 1923.28 386.51,Point2 1923.3 386.67,Point2 1923.6 387.88,Point2 1923.89 387.99,Point2 1925.99 388.28,Point2 1926.58 388.33,Point2 1928.52 388.46,Point2 1929.04 388.43,Point2 1929.53 388.37]"
