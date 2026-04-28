{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Triangle.InTriangleSpec
  ( spec
  ) where

import           System.OsPath
import           Control.Lens
import           Data.Foldable
import           Data.Foldable1
import           HGeometry
import           Data.Maybe
import           HGeometry.Triangle
import           HGeometry.HalfSpace
import           HGeometry.Line
import           HGeometry.Polygon
import           HGeometry.Boundary
import qualified HGeometry.Sign as Sign
import           HGeometry.Ext
import           HGeometry.Instances
import           Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Golden
import           Ipe
import           Ipe.Color
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
      it ("inTriangle: " <> show tri <> " " <> show p) $
        inTriangle (p^.symbolPoint) tri `shouldBe` answer (triAts^?_Attr SStroke)
                                                          (pAts^?_Attr SStroke)
                                                          (p^.symbolName)

answer                :: Maybe (IpeColor R) -> Maybe (IpeColor R) -> Text -> PointLocationResult
answer triCol ptCol sym
  | triCol == ptCol && sym == "mark/disk(sx)" = Inside
  | triCol == ptCol && sym == "mark/box(sx)"  = OnBoundary
  | otherwise                                 = Outside


-- | Test where the query point lies with respect to the triangle
inTriangle   :: ( Point_ corner 2 r
                , Point_ point 2 r, Ord r, Num r, Triangle_ triangle corner)
             => point -> triangle -> PointLocationResult
inTriangle q = foldMap1 (q `inHalfSpace`) . intersectingHalfPlanes


instance Semigroup PointLocationResult where
  -- ^ The semigroup instance essentially interrsects the various results
  Inside     <> x = x
  Outside    <> _ = Outside
  OnBoundary <> x = case x of
                      Outside    -> Outside
                      Inside     -> OnBoundary
                      OnBoundary -> OnBoundary


-- | Test if a point lies inside a halfspace
inHalfSpace     :: ( Point_ point d r, Ord r, Num r
                   , HalfSpace_ halfSpace d r
                   , HyperPlane_ (BoundingHyperPlane halfSpace d r) d r
                   )
                => point -> halfSpace -> PointLocationResult
inHalfSpace q h = case q `onSideTest` (h^.boundingHyperPlane) of
                    LT -> case h^.halfSpaceSign of
                            Sign.Negative -> Inside
                            Sign.Positive -> Outside
                    GT -> case h^.halfSpaceSign of
                            Sign.Negative -> Outside
                            Sign.Positive -> Inside
                    EQ -> OnBoundary
