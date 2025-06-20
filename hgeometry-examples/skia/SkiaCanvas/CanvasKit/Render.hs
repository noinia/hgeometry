{-# LANGUAGE OverloadedStrings          #-}
module SkiaCanvas.CanvasKit.Render
  ( Render
  , renderWith
  , clear
  , liftR
  , withPaint
  -- , withPath

  , picture

  , point
  , circle
  , polyLine
  , lineSegment
  , simplePolygon
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.List.NonEmpty (NonEmpty(..))
import           HGeometry.Ball
import           HGeometry.LineSegment.Class
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Simple
import           Language.Javascript.JSaddle (JSM)
import           SkiaCanvas.CanvasKit.Core (SkCanvas_)
import qualified SkiaCanvas.CanvasKit.Core as CKCore
import qualified SkiaCanvas.CanvasKit.GeomPrims as CKCore
import           SkiaCanvas.CanvasKit.Initialize
import           SkiaCanvas.CanvasKit.Paint (SkPaintRef)
import qualified SkiaCanvas.CanvasKit.Paint as Paint
import qualified SkiaCanvas.CanvasKit.Path as Path
import           SkiaCanvas.CanvasKit.Picture (SkPictureRef, drawPicture)
--------------------------------------------------------------------------------


newtype Render skCanvas a = Render { runRender :: ReaderT (CanvasKitRefs skCanvas) JSM a }
  deriving newtype (Functor,Applicative,Monad, MonadReader (CanvasKitRefs skCanvas))

-- instance MonadTrans (Render skCanvas) where
--   lift = Render . lift

renderWith          :: CanvasKitRefs skCanvas -> Render skCanvas a -> JSM a
renderWith ckRefs r = runReaderT (runRender r) ckRefs


liftR :: JSM a -> Render skCanvas a
liftR = Render . lift

withPaint   :: (SkPaintRef -> Render skCanvas a) -> Render skCanvas a
withPaint f = do canvasKit <- asks (^.canvasKitRef)
                 ckRefs    <- ask
                 liftR $ Paint.withPaint canvasKit (renderWith ckRefs . f)

-- withPath   :: (SkPathRef -> Render skCanvas a) -> Render skCanvas a
-- withPath f = do canvasKit <- asks (^.canvasKitRef)
--                 ckRefs    <- ask
--                 Path.withPath canvasKit (renderWith ckRefs . f)

--------------------------------------------------------------------------------



clear :: SkCanvas_ skCanvas => Render skCanvas ()
clear = do canvasKit <- asks (^.canvasKitRef)
           canvasRef <- asks (^.theCanvas)
           liftR $ CKCore.clear canvasKit canvasRef

--------------------------------------------------------------------------------

-- | Renders a skia picture
picture     :: SkCanvas_ skCanvas => SkPictureRef -> Render skCanvas ()
picture pic = do canvas <- asks (^.theCanvas)
                 liftR $ drawPicture canvas pic

--------------------------------------------------------------------------------

-- | Renders a Point
point             :: forall point r skCanvas.
                     ( Point_ point 2 r
                     , SkCanvas_ skCanvas
                     , Real r
                     )
                  => point -> SkPaintRef -> Render skCanvas ()
point p = circle $ Disk (f $ p^.asPoint) 3
  where
    f :: Point 2 r -> Point 2 Float
    f = over coordinates realToFrac

-- | Renders a circle
circle         :: forall circle point r skCanvas.
                  ( Ball_ circle point
                  , Point_ point 2 r
                  , Real r
                  , SkCanvas_ skCanvas
                  ) => circle -> SkPaintRef -> Render skCanvas ()
circle c paint = let ctr = c^.center.asPoint
                     c'  = Disk (ctr&coordinates %~ toFloat) (toFloat $ c^.squaredRadius)
                     toFloat = realToFrac :: r -> Float
                 in do canvasRef <- asks (^.theCanvas)
                       liftR $ CKCore.circle canvasRef c' paint

-- | Renders a line segment
lineSegment           :: ( LineSegment_ lineSegment point
                         , Point_ point 2 r
                         , Real r
                         , SkCanvas_ skCanvas
                         ) => lineSegment -> SkPaintRef -> Render skCanvas ()
lineSegment seg paint = let cmds = [ Path.MoveTo $ seg^.start.asPoint
                                   , Path.LineTo $ seg^.end.asPoint
                                   ]
                        in do canvasKit <- asks (^.canvasKitRef)
                              canvas    <- asks (^.theCanvas)
                              liftR $ Path.withPathFromCmds canvasKit cmds $ \path ->
                                Path.drawPath canvas path paint

-- | Renders a polyLine
polyLine            :: ( PolyLine_ polyLine point
                       , Point_ point 2 r
                       , Real r
                       , SkCanvas_ skCanvas
                       )
                    => polyLine -> SkPaintRef -> Render skCanvas ()
polyLine poly paint = let (p :| pts) = toNonEmptyOf (vertices.asPoint) poly
                          cmds       = Path.MoveTo p :| map Path.LineTo pts
                      in do canvasKit <- asks (^.canvasKitRef)
                            canvas    <- asks (^.theCanvas)
                            liftR $ Path.withPathFromCmds canvasKit cmds $ \path ->
                              Path.drawPath canvas path paint

-- | Renders a polyLine
simplePolygon            :: ( SimplePolygon_ simplePolygon point r
                            , Real r
                            , SkCanvas_ skCanvas
                            )
                         => simplePolygon -> SkPaintRef -> Render skCanvas ()
simplePolygon poly paint =
  let (p :| pts) = toNonEmptyOf (vertices.asPoint) poly
      cmds       = Path.MoveTo p :| foldr (\q cs -> Path.LineTo q : cs) [Path.Close] pts
  in do canvasKit <- asks (^.canvasKitRef)
        canvas    <- asks (^.theCanvas)
        liftR $ Path.withPathFromCmds canvasKit cmds $ \path ->
          Path.drawPath canvas path paint
