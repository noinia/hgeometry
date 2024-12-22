module Polygon.SeqSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Vector.NonEmpty as NV
import           HGeometry.Cyclic
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Sequence.NonEmpty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------

type R = Int

type MyPolygon point = SimplePolygonF (Cyclic ViewL1) point

myVectorPolygon :: SimplePolygon (Point 2 R)
myVectorPolygon = fromJust . fromPoints $ myPoints

mySeqPolygon :: MyPolygon (Point 2 R)
mySeqPolygon = toSeqPoly myVectorPolygon

-- | Convert into a Seq polygon
toSeqPoly :: Point_ point 2 r => SimplePolygon point -> SimplePolygonF (Cyclic ViewL1) point
toSeqPoly = uncheckedFromCCWPoints . toNonEmptyOf vertices


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

spec :: Spec
spec = describe "Polygon with Seq1 spec" $ do
    it "area2X" $
      area2X myVectorPolygon `shouldBe` area2X mySeqPolygon

    it " rightward traversal 4" $
      let i = 4
          s = 0 :<< Seq.fromList [1,2,3,4,5]
      in
        itoListOf (traverseRightFrom i) s `shouldBe`
        [(4,4),(5,5),(0,0),(1,1),(2,2),(3,3)]

    it " cyclic rightward traversal vector" $
      let i = (-2)
          s = Cyclic $ NV.unsafeFromList [0,1,2,3,4,5]
      in
        itoListOf (traverseRightFrom i) s `shouldBe`
        [(4,4),(5,5),(0,0),(1,1),(2,2),(3,3)]

    it " cyclic rightward traversal seq" $
      let i = (-2)
          s = Cyclic $ 0 :<< Seq.fromList [1,2,3,4,5]
      in
        itoListOf (traverseRightFrom i) s `shouldBe`
        [(4,4),(5,5),(0,0),(1,1),(2,2),(3,3)]

    prop "leftward traversal cyclic seq" $
      let i = 3
          s = Cyclic $ 0 :<< Seq.fromList [1,2,3,4,5]
      in
        itoListOf (traverseLeftFrom i) s `shouldBe`
        [(3,3),(2,2),(1,1),(0,0),(5,5),(4,4)]

    prop "leftward traversal cyclic seq" $
      let i = (-2)
          s = Cyclic $ 0 :<< Seq.fromList [1,2,3,4,5]
      in
        itoListOf (traverseLeftFrom i) s `shouldBe`
        [(4,4),(3,3),(2,2),(1,1),(0,0),(5,5)]


    it " ccwOuterBoundary traversal" $
      let i = 2
          pg = myVectorPolygon
      in
      itoListOf (ccwOuterBoundaryFrom i) pg === itoListOf (ccwOuterBoundaryFrom i) (toSeqPoly pg)


    prop "ccw traversals consistent" $
      \(pg :: SimplePolygon (Point 2 Rational)) (i :: Int) ->
        itoListOf (ccwOuterBoundaryFrom i) pg === itoListOf (ccwOuterBoundaryFrom i) (toSeqPoly pg)

    prop "cw traversals consistent" $
      \(pg :: SimplePolygon (Point 2 Rational)) (i :: Int) ->
        itoListOf (cwOuterBoundaryFrom i) pg === itoListOf (cwOuterBoundaryFrom i) (toSeqPoly pg)

    it "gets rid of duplicate points at the beginning and end" $
      let myPoly :: SimplePolygon (Point 2 (RealNumber 5))
          myPoly = fromJust . fromPoints $
            read  @[Point 2 (RealNumber 5)] "[Point2 0 0,Point2 26 37.1,Point2 7.1 45.2,Point2 (-6.6) 39,Point2 (-1.9) 15.1,Point2 (-1.4) 12.7,Point2 0 0]"
      in myPoly^..vertices `shouldBe` [Point2 26 37.1,Point2 7.1 45.2,Point2 (-6.6) 39,Point2 (-1.9) 15.1,Point2 (-1.4) 12.7,Point2 0 0]
