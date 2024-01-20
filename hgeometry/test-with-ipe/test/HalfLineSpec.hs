{-# LANGUAGE QuasiQuotes #-}
module HalfLineSpec(spec) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Golden
import           HGeometry.Box
import           HGeometry.HalfLine
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

import Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 10


-- getDataFileName' :: OsPath -> IO OsPath
-- getDataFileName' = decodeFS >=> getDataFileName >=> encodeFS

spec :: Spec
spec = describe "halfLine rendering tests" $ do
         goldenWith [osp|data/test-with-ipe/golden/|]
                    (ipeContentGolden { name = [osp|halfLine|] })
                    ( iO' myBox
                    : F.toList (fmap iO' halfLines)
                    )

halfLines :: NonEmpty (HalfLine (Point 2 R))
halfLines = traceShowId $
            (halfLineThrough $ Point2 50 100)
          <$> (sampleBoundary 3 myBox)


myBox :: Rectangle (Point 2 R)
myBox = Rectangle origin (Point2 400 400)


-- | Samples the boundary of the rectangle.
sampleBoundary           :: (Point_ point 2 r, Num r, Ord r)
                         => r -> Rectangle point -> NonEmpty (Point 2 r)
sampleBoundary delta box = sample h t <> sample v r <> sample h b <> sample v l
  where
    Sides t r b l = sides box
    h = Vector2 delta 0
    v = Vector2 0 delta
    sample u seg = NonEmpty.unfoldr gen 0
      where
        gen i = let w = i *^ u
                in ( (seg^.start.asPoint) .+^ w
                   , if quadrance w <= lenV then Just (i+1) else Nothing
                   )
        lenV = quadrance $ (seg^.end) .-. (seg^.start)
