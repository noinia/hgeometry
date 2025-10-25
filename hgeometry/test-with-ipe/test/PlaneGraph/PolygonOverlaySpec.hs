{-# LANGUAGE QuasiQuotes #-}
module PlaneGraph.PolygonOverlaySpec where

import Data.Colour.SRGB (RGB(..))
import HGeometry.HyperPlane
import HGeometry.Unbounded
import Data.Proxy
import Data.Maybe
import Data.Foldable
import Wavefront qualified
import Wavefront(elValue, elMtl)
import Codec.Wavefront.Element qualified as Element
import Codec.Wavefront.Material.Type qualified as Material
import HGeometry.ByIndex
import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Default.Class
import Data.Foldable1
import Data.Foldable1.WithIndex
import Data.Functor.Apply as Apply
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.NonEmpty qualified as NEMap
import Data.Ord (comparing)
import Data.Semialign
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Golden
import HGeometry.Box (Box(..), Rectangle, Corners(..))
import HGeometry.Box qualified as Box
import HGeometry.Ext
import HGeometry.Foldable.Util
import HGeometry.Graphics.Camera
import HGeometry.HyperPlane.NonVertical
import HGeometry.Instances ()
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.LineSegment.Intersection.BentleyOttmann
import HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import HGeometry.Map.NonEmpty.Monoidal qualified as MonoidalNEMap
import HGeometry.Number.Real.Rational
import HGeometry.Number.Radical
import HGeometry.Plane.LowerEnvelope
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as BruteForce
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.Polygon
import HGeometry.Polygon.Convex.Instances ()
import HGeometry.Polygon.Simple
import HGeometry.Polygon.WithHoles
import HGeometry.Properties
import HGeometry.Sequence.NonEmpty (ViewL1(..), asViewL1, singletonL1)
import HGeometry.Transformation
import HGeometry.Triangle
import HGeometry.Interval.Class
import HGeometry.Vector
import HGeometry.VoronoiDiagram
import HGeometry.VoronoiDiagram qualified as VD
import HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import Hiraffe.PlanarGraph.Connected
import Ipe
import Ipe.Color
import Plane.Overlay
import Plane.RenderProps
import PlaneGraph.RenderSpec
import Prelude hiding (zipWith)
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Overlaying Polygons" $ do
      goldenWith [osp|data/test-with-ipe/golden/PlaneGraph/|]
            (ipeFileGolden { name = [osp|planeGraphFromIntersectingSegments|]
                           }
            )
            ( let myPlaneGraph = polygonOverlay myPolygons
                  getZ       _ = view (extra._1)
                  content'     = renderGraph (assignRenderingAttributes getZ myPlaneGraph)
              in addStyleSheet opacitiesStyle $ singlePageFromContent content'
            )
  where
    myTriangles2 :: NonEmpty (Triangle (Point 2 R) :+ (Int, RenderProps))
    myTriangles2 = NonEmpty.fromList
      [ Triangle origin (Point2 100 0) (Point2 100 100)            :+ props 1 black  red
      , Triangle (Point2 10 (-5)) (Point2 20 (-5)) (Point2 20 200) :+ props 2 green blue
      ] -- these triangles are indeed in CCW order....
    myPolygons = myTriangles2&mapped.core %~ fromTriangle
    props z s f = (z, RenderProps (Just $ attr SStroke s) (Just $ attr SFill f))

--------------------------------------------------------------------------------
