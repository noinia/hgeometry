{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Triangle.InTriangleSpec
  ( spec
  ) where

import           System.OsPath
import           Control.Lens
import           Data.Foldable
import           HGeometry
import           HGeometry.Boundary
import           HGeometry.Ext
import           Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Golden
import           Ipe
import           Ipe.Color
import           R
import           Data.Text (Text)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "inTriangle tests" $ do
         ipeTest [osp|pointInTriangle.ipe|]

ipeTest inFp = do
  (points, triangles) <- runIO $ do
        inFp' <- getDataFileName ([osp|test-with-ipe/Triangle/|] <> inFp)
        (points' :: NonEmpty (IpeSymbol R :+ _))  <- NonEmpty.fromList <$> readAllFrom inFp'
        (domains' :: NonEmpty (Triangle (Point 2 R) :+ _)) <- NonEmpty.fromList <$> readAllFrom inFp'
        pure (points',domains')

  for_ triangles $ \(tri :+ triAts) -> do
    for_ points $ \(p :+ pAts) ->
      it ("inTriangle: " <> show tri <> " " <> show p) $
        inTriangle (p^.symbolPoint) tri `shouldBe` answer (triAts^?_Attr SStroke)
                                                          (pAts^?_Attr SStroke)
                                                          (p^.symbolName)

answer                :: Maybe (IpeColor R) -> Maybe (IpeColor R) -> Text -> PointLocationResult
answer triCol ptCol sym
  | triCol == ptCol && sym == "mark/disk(sx)" = Inside
  | triCol == ptCol && sym == "mark/box(sx)"  = OnBoundary
  | otherwise                                 = Outside



inTriangle q tri = undefined
