module Plane.NaiveLowerEnvSpec
  ( spec
  ) where


import           Control.Lens
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Golden
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import qualified HGeometry.Plane.LowerEnvelope.Connected.Randomized as Randomized
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Sequence.Alternating (separators)
import           HGeometry.VoronoiDiagram
import qualified HGeometry.VoronoiDiagram as VD
import           Hiraffe.Graph.Class
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()
import           System.Random

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "lower envelope in bounded region" $ do
         pure ()
