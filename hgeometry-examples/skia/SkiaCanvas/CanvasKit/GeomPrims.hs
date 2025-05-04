{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.GeomPrims
  ( SkInputRect_
  , SkInputRectRef
  , SkInputIRectRef
  , ltrbRect
  , fromRect

  , circle
  ) where

import           Control.Lens
import           Control.Monad (void)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Number.Radical (Radical)
import           HGeometry.Point
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js4)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core
import           SkiaCanvas.CanvasKit.Paint

--------------------------------------------------------------------------------

-- | Rectangles with intager coordinates.
newtype SkInputIRectRef = SkInputIRect JSVal
  deriving (ToJSVal, JS.MakeObject)

-- TODO: we may want to be able to create these things ....

--------------------------------------------------------------------------------

-- | A reference to an SkPath
newtype SkInputRectRef = SkInputRectRef JSVal
  deriving (ToJSVal, JS.MakeObject)

-- | Creates a rectangle
ltrbRect                   :: (ToJSVal r)
                           => CanvasKit -> r ->  r  -> r -> r -> JSM SkInputRectRef
ltrbRect canvasKit l t r b =
  SkInputRectRef <$> canvasKit ^.js4 ("LTRBRect" :: MisoString) l t r b

class ToJSVal rect => SkInputRect_ rect
  -- there is no implementation since all these things are just newtype's over JsVals
  -- anyway.

instance SkInputRect_ SkInputRectRef

-- | Convert a rectangle into a SkRect
--
-- FIXME: since Skia considers the top-left to be the origin, this may produce unexpected
-- results.
fromRect                :: ( Rectangle_ rectangle point
                           , Point_ point 2 r, ToJSVal r
                           )
                        => CanvasKit -> rectangle -> JSM SkInputRectRef
fromRect canvasKit rect = let Sides t r b l = sideValues rect
                          in ltrbRect canvasKit l t r b

--------------------------------------------------------------------------------
-- * Simple objects (Rectangles, Circles)

circle                :: ( Ball_ circle (Point 2 r)
                         , ToJSVal r
                         , Radical r
                         , SkCanvas_ skCanvas
                         ) => skCanvas -> circle -> SkPaintRef -> JSM ()
circle canvas c paint = do Point2 x y <- (c^.center)&coordinates %%~ toJSVal
                           r          <- toJSVal $ radius c
                           void $ canvas ^.js4 ("drawCircle" :: MisoString) x y r paint
