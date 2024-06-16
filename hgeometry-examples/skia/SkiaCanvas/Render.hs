module SkiaCanvas.Render
  ( Drawable(..)
  , Render
  , renderWith
  , CanvasKitRefs(CanvasKitRefs), theCanvasKit, theSurface, strokeOnly, fillOnly, theCanvas
  , HasCanvasKitRef(..)
  , withPaint
  , liftR


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
import           SkiaCanvas.CanvasKit.Initialize (CanvasKitRefs(..), theSurface, strokeOnly, fillOnly, theCanvasKit, theCanvas, HasCanvasKitRef(..))
import           SkiaCanvas.CanvasKit.Paint (SkPaintRef)
import           SkiaCanvas.CanvasKit.Render (Render, liftR, renderWith
                                             , withPaint
                                             )
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
                              , SkCanvas_ skCanvas
                              )
                           => (geom -> SkPaintRef -> Render skCanvas ())
                              -- ^ raw render function
                           -> Canvas r -> geom -> SkPaintRef
                           -> Render skCanvas ()
drawTransform draw' c geom = draw' (toHostFrom (c^.theViewport) geom)


-- | Renders a Point
point   :: ( Point_ point 2 r
           -- , HasCoordinates point (Point 2 Float)
           , RealFrac r
           , IsTransformable point
           , SkCanvas_ skCanvas
           )
        => Canvas r -> point -> SkPaintRef -> Render skCanvas ()
point = drawTransform Render.point

-- | Renders a circle
-- circle :: forall circle point r.
--           ( Ball_ circle point
--           , Point_ point 2 r
--           , RealFrac r
--           ) => Canvas r -> SkCanvasRef -> circle -> SkPaintRef -> JSM ()
-- circle = drawTransform Render.circle


lineSegment :: ( LineSegment_ lineSegment point
               , Point_ point 2 r
               , IsTransformable lineSegment
               , RealFrac r
               , SkCanvas_ skCanvas
               ) => Canvas r -> lineSegment -> SkPaintRef -> Render skCanvas ()
lineSegment = drawTransform Render.lineSegment

-- | Renders a polyLine
polyLine :: ( PolyLine_ polyLine point
            , IsTransformable polyLine
            , Point_ point 2 r
            , RealFrac r
            , SkCanvas_ skCanvas
            )
         => Canvas r -> polyLine -> SkPaintRef -> Render skCanvas ()
polyLine = drawTransform Render.polyLine

-- | Renders a simple polygon
simplePolygon :: ( SimplePolygon_ simplePolygon point r
                 , IsTransformable simplePolygon
                 , RealFrac r
                 , SkCanvas_ skCanvas
                 )
              => Canvas r -> simplePolygon -> SkPaintRef -> Render skCanvas ()
simplePolygon = drawTransform Render.simplePolygon
