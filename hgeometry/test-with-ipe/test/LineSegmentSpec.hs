{-# LANGUAGE QuasiQuotes #-}
module LineSegmentSpec where

import Control.Lens
import Control.Monad ((>=>))
import Data.Bifunctor
import Data.Vinyl
import HGeometry.Boundary
import HGeometry.Box
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.LineSegment
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Vector
import Ipe
import Ipe.Color
import Paths_hgeometry
import System.OsPath
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

getDataFileName' :: OsPath -> IO OsPath
getDataFileName' = decodeFS >=> getDataFileName >=> encodeFS


spec :: Spec
spec = pure ()
{-
  describe "linesegment x box intersection tests" $ do
    fp <- runIO $ getDataFileName' [osp|LineSegment/linesegmentBoxIntersections.ipe|]
    ipeIntersectionTests fp


ipeIntersectionTests    :: OsPath -> Spec
ipeIntersectionTests fp = do (segs,boxes) <- runIO $ (,) <$> readAllFrom fp <*> readAllFrom fp
                             sequence_ $ [ mkTestCase (arrowAsOpen seg) box'
                                         | seg <- segs, box' <- boxes ]
  where
    mkTestCase :: LineSegment AnEndPoint (Point 2 R) :+ IpeAttributes Path R
               -> Rectangle (Point 2 R) :+  IpeAttributes Path R
               -> Spec
    mkTestCase (seg :+ segAts) (rect :+ rectAts) =
      describe ("seg x rect intersection " <> show seg <> " X " <> show rect) $ do
        it "intersects rect" $
          (seg `intersects` rect) `shouldBe` sameColor segAts rectAts
        it "intersects boundary" $
          (seg `intersects` (Boundary rect)) `shouldBe` (sameColor segAts rectAts && notOrange segAts )

-}
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
