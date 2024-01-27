{-# LANGUAGE QuasiQuotes #-}
module LineSegmentSpec where

import Control.Lens
import Control.Monad ((>=>))
import Data.Vinyl
import HGeometry.Box
import HGeometry.Boundary
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.LineSegment
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.HyperPlane.Class
import Ipe
import Ipe.Color
import Paths_hgeometry
import System.OsPath
import Test.Hspec
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

getDataFileName' :: OsPath -> IO OsPath
getDataFileName' = decodeFS >=> getDataFileName >=> encodeFS


spec :: Spec
spec =
  describe "linesegment x box intersection tests" $ do
    it " seg1 intersect top " $
      (seg1 `intersects` topSide') `shouldBe` True
    it "bug" $
      (supportingLine seg1 `intersects` topSide') `shouldBe` True
    it "bug2" $
      (supportingLine topSide' `intersects` seg1) `shouldBe` True
    it "bug2b"   $
      let l = supportingLine topSide'
          eq = hyperPlaneEquation l
          x = evalHyperPlaneEquation l (seg1^.end)
      in
      (eq, x, l, onSideTest (seg1^.start) l, onSideTest (seg1^.end) l) `shouldBe` (eq, x, l,LT,GT)
    -- it "bug3" $
    --   (spansIntersect seg1 topSide') `shouldBe` True


    it " seg2 intersect left " $
      (seg2 `intersects` leftSide') `shouldBe` False
    it "bug4" $
      (supportingLine seg2 `intersects` leftSide') `shouldBe` True
    it "bug5" $
      (supportingLine leftSide' `intersects` seg2) `shouldBe` False
    -- it "bug6" $
    --   (spansIntersect seg2 leftSide') `shouldBe` False



    fp <- runIO $ getDataFileName' [osp|test-with-ipe/LineSegment/linesegmentBoxIntersections.ipe|]
    ipeIntersectionTests fp


ipeIntersectionTests    :: OsPath -> Spec
ipeIntersectionTests fp = do (segs,boxes) <- runIO $ (,) <$> readAllFrom fp <*> readAllFrom fp
                             it "seg x rect intersection" $
                               sequence_ $ [ mkTestCase (arrowAsOpen seg) box'
                                           | seg <- segs, box' <- boxes ]
                             it "seg boundary x rect intersection" $
                               sequence_ $ [ mkTestCaseB (arrowAsOpen seg) box'
                                           | seg <- segs, box' <- boxes ]

  where
    mkTestCase :: LineSegment AnEndPoint (Point 2 R) :+ IpeAttributes Path R
               -> Rectangle (Point 2 R) :+  IpeAttributes Path R
               -> Expectation
    mkTestCase (seg :+ segAts) (rect' :+ rectAts) =
          (seg `intersects` rect') `shouldBe` sameColor segAts rectAts
    mkTestCaseB :: LineSegment AnEndPoint (Point 2 R) :+ IpeAttributes Path R
               -> Rectangle (Point 2 R) :+  IpeAttributes Path R
               -> Expectation
    mkTestCaseB (seg :+ segAts) (rect' :+ rectAts) =
      (seg `intersects` (Boundary rect')) `shouldBe` (sameColor segAts rectAts && notOrange segAts )

sameColor           :: IpeAttributes Path R -> IpeAttributes Path R -> Bool
sameColor atsA atsB = atsA^?_Attr SStroke == atsB^?_Attr SStroke

notOrange     :: IpeAttributes Path R -> Bool
notOrange ats = ats^?_Attr SStroke /= Just orange


-- | interpret an andpoint that has an arrow as an open endpoint.
arrowAsOpen    :: forall r. LineSegment AnEndPoint (Point 2 r) :+ IpeAttributes Path r
               -> LineSegment AnEndPoint (Point 2 r) :+ IpeAttributes Path r
arrowAsOpen ((LineSegment_ p q) :+ ats) =
    LineSegment (f SRArrow p) (f SArrow q) :+ ats
  where
    f   :: at ∈ AttributesOf Path => proxy at -> Point 2 r -> AnEndPoint (Point 2 r)
    f x = case ats^?_Attr x of
            Just _  -> AnOpenE
            Nothing -> AnClosedE


seg1 :: LineSegment AnEndPoint (Point 2 R)
seg1 = read "LineSegment (AnEndPoint Closed (Point2 160 720)) (AnEndPoint Closed (Point2 192 768))"
topSide' :: ClosedLineSegment (Point 2 R)
topSide' = ClosedLineSegment (Point2 128 736) (Point2 256 736)

-- X Box (Point2 128 736) (Point2 256 688), intersects rect


seg2 :: LineSegment AnEndPoint (Point 2 R)
seg2 = read "LineSegment (AnEndPoint Closed (Point2 256 572)) (AnEndPoint Open (Point2 320 584))"

leftSide' :: ClosedLineSegment (Point 2 R)
leftSide' = ClosedLineSegment (Point2 320 560) (Point2 320 624)
-- box' = "Box (Point2 320 560) (Point2 448 624)"



-- seg3= read "LineSegment (AnEndPoint Closed (Point2 256 572)) (AnEndPoint Open (Point2 320 584))"

-- X Box (Point2 320 560) (Point2 448 624),
