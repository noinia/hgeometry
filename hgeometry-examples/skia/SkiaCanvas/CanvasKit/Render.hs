{-# LANGUAGE OverloadedStrings          #-}
module SkiaCanvas.CanvasKit.Render
  ( point
  , circle
  , polyLine
  , simplePolygon
  ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Ball
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Simple
import           Miso (JSM)
import           SkiaCanvas.CanvasKit
import           SkiaCanvas.CanvasKit.Core (SkPaintRef)
import qualified SkiaCanvas.CanvasKit.Core as CKCore

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

-- | Renders a polyLine
simplePolygon                             :: ( SimplePolygon_ simplePolygon point r
                                             , Real r
                                             )
                                          => CanvasKit -> SkCanvasRef
                                          -> simplePolygon -> SkPaintRef
                                          -> JSM ()
simplePolygon canvasKit canvas poly paint =
  let (p :| pts) = toNonEmptyOf (vertices.asPoint) poly
      cmds       = CKCore.MoveTo p :| foldr (\q cs -> CKCore.LineTo q : cs) [CKCore.Close] pts
  in CKCore.withPathFromCmds canvasKit cmds $ \path -> do
       CKCore.drawPath canvas path paint
