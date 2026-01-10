module GiftwrapSpec
  ( spec
  , main
  ) where

import Control.Lens
import Data.Maybe
import Golden
import HGeometry.Ball
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Point
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile
import Test.QuickCheck.Instances ()
import R
import Wavefront qualified
import Codec.Wavefront.Object (WavefrontOBJ, objLocations)
import Data.Vector qualified as Vector

--------------------------------------------------------------------------------


-- byteStringGolden

spec :: Spec
spec = describe "giftwrap tests" $ do
         pure ()
         -- goldenWith [osp|data/test-with-ipe/ConvexHull/|]
         --   (ipeFileGolden { name      = [osp||] }
         --   )
         --   ( let myLines'     = (iO . defIO) <$> myLines


main :: IO ()
main = run [osp|data/test-with-ipe/golden/Render/cube/cube.obj|]
           [osp|data/test-with-ipe/ConvexHull/cube.out.obj|]


run inFP outFp = Wavefront.fromFile inFP >>= \case
    Left err  -> error err
    Right obj -> case NonEmpty.nonEmpty (toList $ allLocations obj) of
                   Nothing  -> error "no triangles?"
                   Just res -> pure res


allLocations :: WavefrontOBJ -> Vector.Vector (Point 3 R)
allLocations = fmap toPoint3 . Wavefront.objLocations
  where
    toPoint3 l = let p = l^.asPoint.to projectPoint
                 in p&coordinates %~ realToFrac
