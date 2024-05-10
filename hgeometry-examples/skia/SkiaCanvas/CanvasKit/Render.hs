module SkiaCanvas.CanvasKit.Render
  ( point
  , circle
  , polyLine
  ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           GHCJS.Marshal (fromJSVal)
import           HGeometry.Ball
import           HGeometry.Miso.Svg.Canvas (HasMousePosition(..))
import           HGeometry.Miso.Svg.StaticCanvas (HasDimensions(..))
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso (Attribute, View, Effect, noEff, onMouseLeave, canvas_, id_, getElementById, JSM)
import           Miso.String (MisoString)
import           MouseExtra
import           SkiaCanvas.CanvasKit
import           SkiaCanvas.CanvasKit.Core (CanvasKit, SkCanvasRef, SkPaintRef)
import qualified SkiaCanvas.CanvasKit.Core as CKCore
import           SkiaCanvas.Core
--------------------------------------------------------------------------------

-- | Renders a Point
point             :: forall point r canvasKit.
                     ( Point_ point 2 r
                     , HasCoordinates point (Point 2 Float)
                     , Real r
                     )
                  => canvasKit -> SkCanvasRef -> point -> SkPaintRef -> JSM ()
point ck canvas p = circle ck canvas $ Disk (f p) 3
  where
    f :: point -> Point 2 Float
    f = over coordinates realToFrac

-- | Renders a circle
circle            :: forall circle point r canvasKit.
                     ( Ball_ circle point
                     , Point_ point 2 r
                     , HasCoordinates point (Point 2 Float)
                     , Real r
                     ) => canvasKit -> SkCanvasRef -> circle -> SkPaintRef -> JSM ()
circle _ canvas c = let ctr = c^.center
                        c'  = Disk (ctr&coordinates %~ toFloat) (toFloat $ c^.squaredRadius)
                        toFloat = realToFrac :: r -> Float
                    in CKCore.circle canvas c'

-- | Renders a polyLine
polyLine                             :: ( PolyLine_ polyLine point
                                        , Point_ point 2 r
                                        , Real r
                                        )
                                     => CanvasKit -> SkCanvasRef -> polyLine -> SkPaintRef
                                     -> JSM ()
polyLine canvasKit canvas poly paint = let (p :| pts) = toNonEmptyOf (vertices.asPoint) poly
                                           cmds       = CKCore.MoveTo p :| map (CKCore.LineTo) pts
                                       in CKCore.withPathFromCmds canvasKit cmds $ \path ->
                                            CKCore.drawPath canvas path paint
