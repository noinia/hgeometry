{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Path
  ( SkPathRef
  , Cmd(..)
  , withPath
  , withPathFromCmds
  , drawPath
  ) where

import           Color (Color, ColorF(..), Alpha(..), fromAlpha)
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.Colour.SRGB
import           Data.Functor.Apply (Apply(..))
import           Data.Word (Word8)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Ball
import           HGeometry.Number.Radical (Radical)
import           HGeometry.Point
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js1, js2, js4, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core
import           SkiaCanvas.CanvasKit.Paint

--------------------------------------------------------------------------------
-- * Path's

-- | A reference to an SkPath
newtype SkPathRef = SkPathRef JSVal
  deriving (ToJSVal, JS.MakeObject)


-- | Apply some operations with a path reference.
withPath           :: CanvasKit -> (SkPathRef -> JSM a) -> JSM a
withPath canvasKit =
    JSAddle.bracket (SkPathRef <$> (JS.new (canvasKit JS.! ("Path" :: MisoString)) ()))
                    (\path -> path ^. JS.js0 ("delete" :: MisoString))

-- | Apply some operations with a path reference.
withPathFromCmds                :: (Traversable f, Real r)
                                => CanvasKit
                                -> f (Cmd r)
                                -> (SkPathRef -> JSM a) -> JSM a
withPathFromCmds canvasKit cmds =
    JSAddle.bracket (do cmds'    <- concat <$> traverse (fromCmd canvasKit) cmds
                        cmdArray <- toJSVal cmds'
                        pathObj  <- canvasKit JS.! ("Path" :: MisoString)
                        SkPathRef <$> (pathObj ^.js1 ("MakeFromCmds" :: MisoString) cmdArray)
                    )
                    (\path -> path ^. JS.js0 ("delete" :: MisoString))

data Cmd r = MoveTo  (Point 2 r)
           | LineTo  (Point 2 r)
           | QuadTo  (Point 2 r) (Point 2 r)
           | ConicTo (Point 2 r) (Point 2 r) r
           | CubicTo (Point 2 r) (Point 2 r) (Point 2 r)
           | Close
           deriving (Show,Eq)

instance Functor Cmd where
  fmap f = \case
    MoveTo p      -> MoveTo $ p&coordinates %~ f
    LineTo p      -> LineTo $ p&coordinates %~ f
    QuadTo p q    -> QuadTo (p&coordinates %~ f) (q&coordinates %~ f)
    ConicTo p q w -> ConicTo (p&coordinates %~ f) (q&coordinates %~ f) (f w)
    CubicTo p q r -> CubicTo (p&coordinates %~ f) (q&coordinates %~ f) (r&coordinates %~ f)
    Close         -> Close

fromCmd   ::  Real r => CanvasKit -> Cmd r -> JSM [JSVal]
fromCmd ck = fromFloatCmd ck . fmap realToFrac

fromFloatCmd   :: CanvasKit -> Cmd Float -> JSM [JSVal]
fromFloatCmd _ = \case
    MoveTo p      -> do Point2 x y <- p&coordinates %%~ toJSVal
                        moveTo <- toJSVal moveVerb
                        pure [moveTo, x , y]
    LineTo p      -> do Point2 x y <- p&coordinates %%~ toJSVal
                        lineTo <- toJSVal lineVerb
                        pure [lineTo, x , y]
    QuadTo p q    -> do Point2 px py <- p&coordinates %%~ toJSVal
                        Point2 qx qy <- q&coordinates %%~ toJSVal
                        quadTo <- toJSVal quadVerb
                        pure [quadTo, px , py, qx, qy]
    ConicTo p q w -> do Point2 px py <- p&coordinates %%~ toJSVal
                        Point2 qx qy <- q&coordinates %%~ toJSVal
                        w'           <- toJSVal w
                        conicTo      <- toJSVal conicVerb
                        pure [conicTo, px , py, qx, qy, w']
    CubicTo p q r -> do Point2 px py <- p&coordinates %%~ toJSVal
                        Point2 qx qy <- q&coordinates %%~ toJSVal
                        Point2 rx ry <- r&coordinates %%~ toJSVal
                        cubicTo <- toJSVal cubicVerb
                        pure [cubicTo, px , py, qx, qy, rx, ry]
    Close         -> do close' <- toJSVal closeVerb
                        pure [close']
  where
    moveVerb  = 0 :: Int -- CanvasKit.MOVE_VERB
    lineVerb  = 1 :: Int -- CanvasKit.LINE_VERB
    quadVerb  = 2 :: Int -- CanvasKit.QUAD_VERB
    conicVerb = 3 :: Int -- CanvasKit.CONIC_VERB
    cubicVerb = 4 :: Int -- CanvasKit.CUBIC_VERB
    closeVerb = 5 :: Int -- CanvasKit.CLOSE_VERB

-- |  call drawPath
drawPath                   :: SkCanvas_ skCanvas
                           => skCanvas -> SkPathRef -> SkPaintRef -> JSM ()
drawPath canvas path paint =
  void $ canvas ^.js2 ("drawPath" :: MisoString) path paint
