{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module LineSegment.Intersection.WithHalfSpaceSpec where

import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable (toList, for_)
import           Data.Foldable1
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust, mapMaybe, maybeToList)
import qualified Data.Text as Text
import           Data.Traversable
import           Data.Vector.NonEmpty (NonEmptyVector)
import           Golden
import           HGeometry.Box
import qualified HGeometry.Box as Box
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Interval.EndPoint
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Convex.Internal
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Triangle
import qualified HGeometry.Triangle as Triangle
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

import           Debug.Trace
import           Data.Functor.Classes

--------------------------------------------------------------------------------

type R = RealNumber 5

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
