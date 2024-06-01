{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.WorldSpec (spec) where


import Control.Lens
-- import           Debug.Trace
import HGeometry
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
import HGeometry.Polygon.Class
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Triangulation
import HGeometry.Transformation
import Ipe
import System.OsPath
import Test.Hspec
import Test.Hspec.QquickCheck
import HGeometry.Instances ()
import Paths_hgeometry

-- import           Test.QuickCheck
-- import           Test.QuickCheck.Instances ()

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
                   return $ Country <$> polies'


newtype Country = Country (SimplePolygon (Point 2 R))
             deriving (Show,Eq)

instance Arbitrary Country where
  arbitrary = elements allCountries
  shrink (Country pg) = Country <$> shrink pg

spec :: Spec
spec = describe "triangulate World" $
         prop "triangulate yields only triangles" $
           \(Country pg) -> let subdivision = triangulate pg
                                triangs = subdivision^..traverse.interiorFacePolygons
                            in all isTriangle' triangs

isTriangle'    :: SimplePolygon (Point 2 R) -> Bool
isTriangle' pg = numVertices pg == 3
