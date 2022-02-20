{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.LineSegmentSpec where

import Data.Bifunctor
import Control.Lens
import Data.Ext
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.LineSegment.Internal (onSegment, onSegment2)
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Boundary
import Data.Geometry.Box
import Data.Intersection
import Data.RealNumber.Rational
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Ipe
import Ipe.Color
import Data.Vinyl

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec =
  describe "onSegment" $ do
    let intersects3 :: Point 3 R -> LineSegment 3 () R -> Bool
        intersects3 = intersects
        intersects2 :: Point 2 R -> LineSegment 2 () R -> Bool
        intersects2 = intersects
    testI

    -- it "handles zero length segments correctly" $ do
    --   let zeroSegment = ClosedLineSegment (Point2 0 0 :+ ()) (Point2 0 0 :+ ())
    --   (Point2 0 0 `intersects2` zeroSegment) `shouldBe` True
    --   (Point2 1 0 `intersects2` zeroSegment) `shouldBe` False

    it "2d on segment tests" $ do
      let seg1 = ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ())
          seg2 = ClosedLineSegment (origin :+ ()) (Point2 3 3 :+ ())
      (Point2 1    0 `intersects2` seg1) `shouldBe`  True
      (Point2 1    1 `intersects2` seg1) `shouldBe` False
      (Point2 5    0 `intersects2` seg1) `shouldBe` False
      (Point2 (-1) 0 `intersects2` seg1) `shouldBe` False
      (Point2 1    1 `intersects2` seg2) `shouldBe`  True

    it "3d on segment tests" $ do
      let seg = ClosedLineSegment (origin :+ ()) (Point3 3 3 3 :+ ())
      (Point3 1 1 1 `intersects3` seg) `shouldBe` True
      (Point3 1 2 1 `intersects3` seg) `shouldBe` False

    it "onSegment2 same result as generic onSegment (quickheck; (mostly) false points)" $
      property $ \(q :: Point 2 R) (seg :: LineSegment 2 () R) ->
        q `onSegment` seg == q `onSegment2` seg
        -- note: most of the points above will likely not lie on the segment
    it "onSegment2 same result as generic onSegment (quickheck ; true points)" $
      property $ \(lambda :: R) (seg :: LineSegment 2 () R) ->
        let v = (seg^.end.core) .-. (seg^.start.core)
            q = (seg^.start.core) .+^ (lambda *^ v)
        in q `onSegment` seg == q `onSegment2` seg

    it "intersecting line segment and line" $ do
      let s = ClosedLineSegment (ext origin) (ext $ Point2 10 (0 :: R))
      (s `intersect` horizontalLine (0 :: R)) `shouldBe` coRec s
    describe "linesegment x box intersection tests" $
      ipeIntersectionTests "src/Data/Geometry/linesegmentBoxIntersections.ipe"




ipeIntersectionTests    :: FilePath -> Spec
ipeIntersectionTests fp = do (segs,boxes) <- runIO $ (,) <$> readAllFrom fp <*> readAllFrom fp
                             sequence_ $ [ mkTestCase (arrowAsOpen seg) box'
                                         | seg <- segs, box' <- boxes ]
  where
    mkTestCase :: LineSegment 2 () R :+ IpeAttributes Path R
               -> Rectangle () R :+  IpeAttributes Path R
               -> Spec
    mkTestCase (seg :+ segAts) (rect :+ rectAts) =
      describe ("seg x rect intersection " <> show seg <> " X " <> show rect) $ do
        it "intersects rect" $
          (seg `intersects` rect) `shouldBe` sameColor segAts rectAts
        it "intersects boundary" $
          (seg `intersects` (Boundary rect)) `shouldBe` (sameColor segAts rectAts && notOrange segAts )


sameColor           :: IpeAttributes Path R -> IpeAttributes Path R -> Bool
sameColor atsA atsB = atsA^?_Attr SStroke == atsB^?_Attr SStroke

notOrange     :: IpeAttributes Path R -> Bool
notOrange ats = ats^?_Attr SStroke /= Just orange


-- | interpret an andpoint that has an arrow as an open endpoint.
arrowAsOpen    :: forall p r. LineSegment 2 p r :+ IpeAttributes Path r
               -> LineSegment 2 p r :+ IpeAttributes Path r
arrowAsOpen ((LineSegment' (p :+ a) (q :+ b)) :+ ats) =
    LineSegment (f SRArrow p <&> (:+ a)) (f SArrow q <&> (:+ b)) :+ ats
  where
    f   :: at ∈ AttributesOf Path => proxy at -> Point 2 r -> EndPoint (Point 2 r)
    f x = case ats^?_Attr x of
            Just _  -> Open
            Nothing -> Closed



test1,test2,test3,test4 :: LineSegment 2 () Int
test1 = ClosedLineSegment (ext $ Point2 0 10) (ext $ Point2 0 20)

test2 = OpenLineSegment (ext $ Point2 0 5) (ext $ Point2 0 20)

test3 = ClosedLineSegment (ext $ Point2 0 21) (ext $ Point2 0 5)

test4 = LineSegment (Open $ ext $ Point2 0 10) (Closed $ ext $ Point2 0 9)

-- test = withRank (Vector2 0 1) test1 test4


testI = describe "some manual intersection tests" $ do
          it "manual intersection" $ (test1 `intersects` test2 ) `shouldBe` True
          it "manual intersection" $ (test1 `intersects` test3 ) `shouldBe` True
          it "manual intersection" $ (test1 `intersects` test4 ) `shouldBe` False
          it "manual intersection" $ (test2 `intersects` test4 ) `shouldBe` True
