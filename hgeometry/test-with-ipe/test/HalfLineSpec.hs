{-# LANGUAGE QuasiQuotes #-}
module HalfLineSpec(spec) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.Box
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Line
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           Ipe
-- import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

-- import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 10


-- getDataFileName' :: OsPath -> IO OsPath
-- getDataFileName' = decodeFS >=> getDataFileName >=> encodeFS

hl1 :: HalfLine (Point 2 R)
hl1 = HalfLine (Point2 50 100) (Vector2 (-50) 8)

-- hl2 :: HalfLine (Point 2 R)
-- hl2 = HalfLine (Point2 50 100) (Vector2 (-50) 47)

bx :: Rectangle (Point 2 R)
bx = Box (Point2 (-1000) (-1000)) (Point2 1000 1000)


spec :: Spec
spec = describe "halfLine rendering tests" $ do
          it "halfLine1 intersects with box" $
            (hl1 `intersects`  bx) `shouldBe` True
          it "halfLine1 intersect with box" $
            let answ = HalfLine_x_Box_LineSegment (ClosedLineSegment (Point2 50 100)
                                                                     (Point2 (-1000) 268))
            in (hl1 `intersect` bx) `shouldBe` Just  answ


          it "line intersect with box" $
            let answ = Line_x_Box_LineSegment (ClosedLineSegment (Point2 (-1000) 268)
                                                                 (Point2 1000 (-52)))
            in (supportingLine hl1 `intersect` bx) `shouldBe` Just  answ


          -- it "halfLine 2 intersects with box" $
          --   (hl2 `intersects`  bx) `shouldBe` True
          -- it "halfLine  2intersect with box" $
          --   let answ = HalfLine_x_Box_LineSegment (ClosedLineSegment (Point2 50 100)
          --                                                            (Point2 (-1000) 268))
          --   in (hl2 `intersect` bx) `shouldBe` Just  answ

          -- it "line2 intersect with box" $
          --   let m = supportingLine hl2
          --       answ = Line_x_Box_LineSegment (ClosedLineSegment (Point2 (-1000) 268)
          --                                                        (Point2 1000 (-52)))
          --   in (m, m `intersect` bx) `shouldBe` (m, Just answ)





          goldenWith [osp|data/test-with-ipe/golden/|]
                     (ipeContentGolden { name = [osp|halfLine|] })
                     ( iO' myBox
                     : iO' (sampleBoundary 3 myBox)
                     : F.toList (fmap iO' halfLines)
                     )

halfLines :: NonEmpty (HalfLine (Point 2 R))
halfLines = (halfLineThrough $ Point2 50 100)
          <$> (sampleBoundary 3 myBox)


myBox :: Rectangle (Point 2 R)
myBox = Rectangle origin (Point2 400 400)


-- | Samples the boundary of the rectangle.
sampleBoundary           :: (Point_ point 2 r, Num r, Ord r)
                         => r -> Rectangle point -> NonEmpty (Point 2 r)
sampleBoundary delta box =
    sample h t <> sample (negated v) r <> sample (negated h) b <> sample v l
  where
    Sides t r b l = sides box
    h = Vector2 delta 0
    v = Vector2 0 delta
    sample u seg = NonEmpty.unfoldr gen 0
      where
        gen i = let w  = i *^ u
                    w' = (i+1) *^ u
                in ( (seg^.start.asPoint) .+^ w
                   , if quadrance w' <= lenV then Just (i+1) else Nothing
                   )
        lenV = quadrance $ (seg^.end) .-. (seg^.start)
