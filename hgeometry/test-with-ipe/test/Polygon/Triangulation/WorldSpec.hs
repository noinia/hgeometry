{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.WorldSpec (spec) where


import Control.Lens hiding (elements)
import Golden (getDataFileName)
import HGeometry
import HGeometry.Ext
import HGeometry.Instances ()
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
import HGeometry.Polygon.Instances (shrinkPolygon)
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Triangulation
import Ipe
import System.IO.Unsafe
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
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
spec = describe "triangulate World" $
         prop "triangulate yields only triangles" $
           \(Country pg) -> let subdivision = triangulate pg
                                triangs = subdivision^..interiorFacePolygons
                            in all isTriangle' triangs

isTriangle'    :: SimplePolygon_ simplePolygon point r => simplePolygon  -> Bool
isTriangle' pg = numVertices pg == 3
