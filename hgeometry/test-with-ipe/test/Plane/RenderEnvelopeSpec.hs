{-# LANGUAGE QuasiQuotes #-}
module Plane.RenderEnvelopeSpec
  where

import           Control.Lens
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec.WithTempFile
import           Golden
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Instances ()
import           HGeometry.Ext
import           HGeometry.Polygon.Convex.Instances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import           HGeometry.VoronoiDiagram
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import           HGeometry.Triangle
import           HGeometry.Point
import qualified HGeometry.VoronoiDiagram as VD
import           HGeometry.Box
import           HGeometry.Transformation
import           HGeometry.Graphics.Camera
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Ipe.Color

--------------------------------------------------------------------------------

type R = RealNumber 5

triangles :: [Triangle (Point 3 Double) :+ IpeColor Double]
triangles = -- scaleUniformlyBy 5 <$>
        [ -- ground plane
          Triangle origin (Point3 1 0 0) (Point3 1 1 0) :+ blue
        , Triangle origin (Point3 1 1 0) (Point3 0 1 0) :+ blue
        -- left side
        , Triangle origin (Point3 0 1 0) (Point3 0 1 1) :+ green
        , Triangle origin (Point3 0 1 1) (Point3 0 0 1) :+ green
        -- front plane
        -- , Triangle origin (Point3 1 0 0) (Point3 1 0 1) :+ red
        -- , Triangle origin (Point3 1 0 1) (Point3 0 1 1) :+ red

        -- back plane
        , Triangle (Point3 0 1 0) (Point3 1 1 0) (Point3 1 1 1) :+ orange
        , Triangle (Point3 0 1 0) (Point3 1 1 1) (Point3 0 1 1) :+ orange
        ]


planes :: NonEmpty (Plane R :+ IpeColor Double)
planes = points&traverse.core %~ pointToPlane

points :: NonEmpty (Point 2 R :+ IpeColor Double)
points = NonEmpty.fromList
         [ Point2 2 3 :+ red
         ]


myCamera :: Camera Double
myCamera = blenderCamera


-- | fit to something that fits in this rectangle
screenBox :: Rectangle (Point 2 Double)
screenBox = Rectangle origin (Point2 500 500)


-- theTransform =

-- instance IsBoxable (IpeObject r) where
--   boundingBox = \case
--     IpeGroup g     -> boundingBox g
--     IpeImage i     -> boundingBox i
--     IpeTextLabel l -> boundingBox l
--     IpeMiniPage p  -> boundingBox p
--     IpeUse u       -> boundingBox u
--     IpePath p      -> boundingBox p

spec :: Spec
spec = describe "Plane.RenderEnvelope"  $ do
         goldenWith [osp|data/test-with-ipe/golden/Plane/|]
           (ipeFileGolden { name = [osp|lowerEnvelopeRender|]
                          }
           )
           ( let content' = let tris = renderToIpe myCamera triangles
                                t    = uniformScaling 1000
                                      --- fitToBoxTransform screenBox  tris -- TODO
                            in transformBy t tris
             in addStyleSheet opacitiesStyle $ singlePageFromContent content'
           )


         --------------------------------------------------------------------------------

renderToIpe       :: forall point r r' f.
                       (Foldable f, Functor f, Point_ point 3 r, Real r, Real r')
                   => Camera r -> f (Triangle point :+ IpeColor r') -> [IpeObject Double]
renderToIpe camera = foldMap asIpe . render camera
  where
    asIpe (triangle :+ col) = [iO $ defIO triangle ! attr SFill (realToFrac <$> col)]

-- | Render a scene; i..e a set of triangles
render        :: forall point r r' f. (Functor f, Point_ point 3 r, Real r)
              => Camera r
              -> f (Triangle point :+ IpeColor r')
              -> f (Triangle (Point 2 Double) :+ IpeColor r')
render camera = over (mapped.core) $ \triangle ->
    triangle&vertices %~
            projectPoint . transformBy (cameraTransform camera') . f
  where
    camera' = realToFrac <$> camera
    f   :: point -> Point 3 Double
    f p = over coordinates realToFrac (p^.asPoint)
