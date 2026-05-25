{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Triangle.InTriangleSpec
  ( spec
  ) where

import           System.OsPath
import           Control.Lens
import           Data.Foldable
import           HGeometry
import           Data.Maybe
import           HGeometry.Triangle
import           HGeometry.Polygon
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Instances()
import           Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Golden
import           Ipe
import           R
import           Data.Text (Text)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "inTriangle tests" $ do
         ipeTest [osp|pointInTriangle.ipe|]
         prop "inTriangle consistent with inPolygon" $
           \(q :: Point 2 R) (tri :: Triangle (Point 2 R)) ->
             inTriangle q tri
             ===
             asPointLocationResult
               (q `inPolygon` fromJust (fromPoints tri :: Maybe (SimplePolygon _)))

         prop "inTriangle consistent with intersects" $
           \(q :: Point 2 R) (tri :: Triangle (Point 2 R)) ->
             case inTriangle q tri of
               Outside -> not $ q `intersects` tri
               _       -> q `intersects` tri

         prop "inHalfSpace consistent with intersects (LinePV)" $
           \(q :: Point 2 R) (h :: HalfPlaneF (LinePV 2 R)) ->
             case inHalfSpace q h of
               Outside -> not $ q `intersects` h
               _       -> q `intersects` h
         prop "inHalfSpace consistent with intersects (LineEQ)" $
           \(q :: Point 2 R) (h :: HalfPlaneF (LineEQ R)) ->
             case inHalfSpace q h of
               Outside -> not $ q `intersects` h
               _       -> q `intersects` h

         prop "inHalfSpace consistent with intersects (GeneralLine)" $
           \(q :: Point 2 R) (h :: HalfPlaneF (VerticalOrLineEQ R)) ->
             case inHalfSpace q h of
               Outside -> not $ q `intersects` h
               _       -> q `intersects` h


ipeTest inFp = do
  (points, triangles) <- runIO $ do
        inFp' <- getDataFileName ([osp|test-with-ipe/Triangle/|] <> inFp)
        (points' :: NonEmpty (IpeSymbol R :+ _))  <- NonEmpty.fromList <$> readAllFrom inFp'
        (domains' :: NonEmpty (Triangle (Point 2 R) :+ _)) <- NonEmpty.fromList <$> readAllFrom inFp'
        pure (points',domains')

  for_ triangles $ \(tri :+ triAts) -> do
    for_ points $ \(p :+ pAts) ->
      it ("inTriangle: " <> show tri <> ", " <> show (p^.symbolPoint)) $
        inTriangle (p^.symbolPoint) tri `shouldBe` answer (triAts^.stroke)
                                                          (pAts^.stroke)
                                                          (p^.symbolName)

answer                :: Maybe (IpeColor R) -> Maybe (IpeColor R) -> Text -> PointLocationResult
answer triCol ptCol sym
  | triCol == ptCol && sym == "mark/disk(sx)" = Inside
  | triCol == ptCol && sym == "mark/box(sx)"  = OnBoundary
  | otherwise                                 = Outside
