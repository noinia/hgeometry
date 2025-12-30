{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module LineSegment.Intersection.WithHalfSpaceSpec where

import           Control.Lens
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.Ext
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.LineSegment
import           R
import           HGeometry.Point
import           HGeometry.Polygon.Instances ()
import           Ipe
import           System.OsPath
import           Test.Hspec


--------------------------------------------------------------------------------

spec :: Spec
spec = describe "LineSegment x HalfSpace intersetion tests" $ do
         testIpe [osp|intersectHalfPlane.ipe|]

testIpe      :: OsPath -> Spec
testIpe inFp = describe ("on file " <> show inFp) $ do
          (halfPlanes, segments) <-  runIO $ loadInputs inFp

          for_ segments $ \seg ->
            for_ halfPlanes $ \halfPlane -> do
              it ("intersects halfplane and line segment") $
                (seg `intersects` halfPlane) `shouldBe` True


loadInputs      :: OsPath -> IO ( NonEmpty (HalfSpaceF (LinePV 2 R) :+ _)
                                , NonEmpty (ClosedLineSegment (Point 2 R) :+ _)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/LineSegment/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
            segs                                           = NonEmpty.fromList $ readAll page
        -- take the left halfplane of every halfline
        pure (over core (leftHalfPlane . asOrientedLine) <$> rays, segs)
