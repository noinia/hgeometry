module SkiaCanvas.Render
  ( Drawable(..)
  , point
  -- , circle
  , polyLine
  , lineSegment
  , simplePolygon
  ) where

import           Control.Lens
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Transformation
import           HGeometry.Viewport
import           Miso (JSM)
import           SkiaCanvas.CanvasKit
import           SkiaCanvas.CanvasKit.Core (SkPaintRef)
import qualified SkiaCanvas.CanvasKit.Render as Render
import           SkiaCanvas.Core

--------------------------------------------------------------------------------

class Drawable geom where
  -- | Draw the given geometry, take care of using the right viewport transform
  draw :: Canvas r -> SkCanvasRef -> geom -> SkPaintRef -> JSM ()

-- instance ( Point_ point 2 r
--          , HasCoordinates point (Point 2 Float)
--          , Real r
--          ) => Drawable (Point 2 r)



drawTransform              :: ( IsTransformable geom
                              , NumType geom ~ r, Dimension geom ~ 2, Fractional r
                              )
                           => (CanvasKit -> SkCanvasRef -> geom -> SkPaintRef -> JSM ())
                              -- ^ raw render function
                           -> Canvas r -> SkCanvasRef -> geom -> SkPaintRef -> JSM ()
drawTransform draw' c ckRef geom =
  draw' (c^?!canvasKitRef._Just) ckRef (toHostFrom (c^.theViewport) geom)
  -- TODO: parameterize canvas with an f


-- | Renders a Point
point   :: ( Point_ point 2 r
           , HasCoordinates point (Point 2 Float)
           , RealFrac r
           , IsTransformable point
           )
        => Canvas r -> SkCanvasRef -> point -> SkPaintRef -> JSM ()
point = drawTransform Render.point

-- | Renders a circle
-- circle :: forall circle point r.
--           ( Ball_ circle point
--           , Point_ point 2 r
--           , HasCoordinates point (Point 2 Float)
--           , RealFrac r
--           ) => Canvas r -> SkCanvasRef -> circle -> SkPaintRef -> JSM ()
-- circle = drawTransform Render.circle


lineSegment :: ( LineSegment_ lineSegment point
               , Point_ point 2 r
               , IsTransformable lineSegment
               , RealFrac r
               ) => Canvas r -> SkCanvasRef -> lineSegment -> SkPaintRef -> JSM ()
lineSegment = drawTransform Render.lineSegment

-- | Renders a polyLine
polyLine :: ( PolyLine_ polyLine point
            , IsTransformable polyLine
            , Point_ point 2 r
            , RealFrac r
            )
         => Canvas r -> SkCanvasRef -> polyLine -> SkPaintRef -> JSM ()
polyLine = drawTransform Render.polyLine

-- | Renders a simple polygon
simplePolygon :: ( SimplePolygon_ simplePolygon point r
                 , IsTransformable simplePolygon
                 , RealFrac r
                 )
              => Canvas r -> SkCanvasRef -> simplePolygon -> SkPaintRef -> JSM ()
simplePolygon = drawTransform Render.simplePolygon
