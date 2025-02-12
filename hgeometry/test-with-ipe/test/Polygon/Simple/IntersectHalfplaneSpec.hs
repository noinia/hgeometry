{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.IntersectHalfplaneSpec(spec) where


import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Bifoldable
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (maybeToList, fromJust)
import           Golden
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Halfspace
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Simple.DualTree
import           HGeometry.Polygon.Simple.Sample
import           HGeometry.Polygon.Simple.ShortestPath.Tree
import           HGeometry.Polygon.Triangulation
import           HGeometry.Properties
import           HGeometry.Trie
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           System.Random
import           System.Random.Stateful
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck

--------------------------------------------------------------------------------


spec :: Spec
spec = describe "simple polygon x halfspace intersection" $ do
         testIpe [osp|polygonHalfspaceIntersection.ipe|]

testIpe            :: OsPath -> OsPath -> Spec
testIpe inFp outFp = describe (show inFp) $ do
    (sources, poly) <-  runIO $ do
        inFp'      <- getDataFileName ([osp|test-with-ipe/Polygon/Simple/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (pts :: NonEmpty (Point 2 R :+ _))            = NonEmpty.fromList $ readAll page
            ((pg :: SimplePolygon (Point 2 R) :+ _) : _)  = readAll page
        pure (pts,pg)

    pure ()
