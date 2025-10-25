{-# LANGUAGE QuasiQuotes #-}
module PlaneGraph.PolygonOverlaySpec
  ( spec

  , assignRenderingAttributes
  , renderGraph
  ) where

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
import HGeometry.PlaneGraph.Connected.PolygonOverlay

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

-- | Convert a triangle into a simple polygon
fromTriangle :: (Point_ vertex 2 r, Ord r, Num r) => Triangle vertex -> SimplePolygon vertex
fromTriangle = fromMaybe (error "fromTriangle: absurd") . fromPoints

--------------------------------------------------------------------------------

-- | Function to draw a plane graph whose faces may have some
-- attributes associated them.
renderGraph    :: forall planeGraph vertex r r'.
                  ( PlaneGraph_ planeGraph vertex, HasOuterBoundaryOf planeGraph
                  , Point_ vertex 2 r
                  , Ord r, Fractional r
                  , Real r'
                  , Face planeGraph ~ Maybe (IpeAttributes Path r')
                  , Edge planeGraph ~ Maybe (IpeAttributes Path r')
                  , Eq (FaceIx planeGraph)
                  , HasInnerComponents planeGraph
                  ) => planeGraph -> [IpeObject r]
renderGraph gr = theFaces <> theEdges
  where
    theEdges = ifoldMapOf edgeSegments         drawEdge' gr
    theFaces = ifoldMapOf interiorFacePolygons drawFace' gr
    drawFace' fi pg = case attrToR <$> gr^?!faceAt fi of
        Nothing  -> []
        Just ats -> [ iO $ ipePolygon pg' ! ats ]
      where
          pg' :: PolygonalDomain _
          pg' = pg&vertices %~ toPoint

    drawEdge' d s = case attrToR <$> gr^?!edgeAt d of
        Nothing  -> []
        Just ats -> [ iO $ ipeLineSegment s' ! ats ]
      where
        s' :: ClosedLineSegment (Point 2 r)
        s' = s&vertices %~ toPoint

    toPoint :: Point_ point 2 r => point -> Point 2 r
    toPoint = view asPoint

    attrToR = mapIpeAttrs (Proxy @Path) realToFrac

--------------------------------------------------------------------------------

-- | Lables the faces and edges with the attributes to use in rendering
--
-- the z-values attached to the edgess/faces determine how to render the edge/face.
-- in particular, we take the value corresponding to the smallest z-value.
assignRenderingAttributes         :: forall s v polygon z r.
                                     (HasRenderProps polygon, Ord z, r ~ NumType polygon)
                                  => (Point 2 r -> polygon -> z)
                                  -> CPlaneGraph s v
                                                   (E polygon)
                                                   (F polygon)
                                  -> CPlaneGraph s v
                                                   (Maybe (IpeAttributes Path Double))
                                                   (Maybe (IpeAttributes Path Double))
assignRenderingAttributes getZ gr = gr1&faces %~ \(F covering mp) ->
                                                   do p    <- mp
                                                      poly <- minimumOn (getZ p) covering
                                                      poly^.faceAttrs
  where
    gr1 :: CPlaneGraph s v
                         (Maybe (IpeAttributes Path Double))
                         (F polygon)
    gr1 = gr&edges %~ \(E defs covering p) -> do
                          _ :+ definer <- minimumOn (getZ p . view extra) defs
                          let zCovering = maybe Top (ValT . getZ p) $ minimumOn (getZ p) covering
                          if ValT (getZ p definer) <= zCovering
                            then definer^.renderProps.edgeAttrs
                            else Nothing
  -- We compute the first defining polygon and its z-value, and the
  -- props corresponding to this edge. Similarly, we compute the
  -- minimum zvalue of the covering regions. If the covering z-value
  -- is smaller than the one among the definers, then we show Nothing;
  -- as the edge is hidden. Otherwise we show the definer's value

-- TODO: maybe we still want to store the z-value, and still draw them in ipe in
-- back to front order.

-- TODO: we are computing getZ on the minimum twice. That seems wasteful


-- | Compute the minimum using a given function
minimumOn   :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
minimumOn f = minimumByOf folded (comparing f)


--------------------------------------------------------------------------------
