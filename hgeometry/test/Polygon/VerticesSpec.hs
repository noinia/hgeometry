module Polygon.VerticesSpec
  ( spec
  ) where

import Control.Lens
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import HGeometry.Ext
import HGeometry.Point
import HGeometry.Polygon.Class
import HGeometry.Polygon.Simple
import Test.Hspec

--------------------------------------------------------------------------------

type R = Int

spec :: Spec
spec = describe "testing vertices" $
         it "using both points and ext points" $
           let (a,b) = test in a `shouldBe` b

foo :: SimplePolygon_ simplePolygon point r => simplePolygon -> NonEmpty (Point 2 r)
foo = toNonEmptyOf (vertices.asPoint)


-- test :: (NonEmpty (Point 2 R), NonEmpty (Point 2 R))
test = ( foo myPolygon
       , foo myPolygon2
       )


myPolygon :: SimplePolygon (Point 2 R)
myPolygon = fromJust . fromPoints $ myPoints


myPoints :: [Point 2 R]
myPoints = [ Point2 64 128
           , Point2 48 64
           , Point2 96 32
           , Point2 128 64
           , Point2 176 48
           , Point2 176 96
           , Point2 80 80
           , Point2 128 128
           , Point2 112 176
           , Point2 48 160
           ]

myPolygon2 :: SimplePolygon (Point 2 R :+ Int)
myPolygon2 = fromJust . fromPoints . fmap ( :+ 5) $ myPoints
