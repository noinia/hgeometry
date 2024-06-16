{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.GeomPrims
  ( SkInputRect_
  , SkInputRectRef
  , lrtbRect

  , circle
  ) where

import           Control.Lens
import           Control.Monad (void)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Ball
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

-- | A reference to an SkPath
newtype SkInputRectRef = SkInputRectRef JSVal
  deriving (ToJSVal, JS.MakeObject)

-- | Creates a rectangle
lrtbRect                   :: (Num r, ToJSVal r)
                           => CanvasKit -> r ->  r  -> r -> r -> JSM SkInputRectRef
lrtbRect canvasKit l r t b =
  SkInputRectRef <$> canvasKit ^.js4 ("LTRBRect" :: MisoString) l t b r

class ToJSVal rect => SkInputRect_ rect
  -- there is no implementation since all these things are just newtype's over JsVals
  -- anyway.

instance SkInputRect_ SkInputRectRef





--------------------------------------------------------------------------------
-- * Simple objects (Rectangles, Circles)

circle                :: ( Ball_ circle (Point 2 r)
                         , ToJSVal r
                         , Radical r
                         , SkCanvas_ skCanvas
                         ) => skCanvas -> circle -> SkPaintRef -> JSM ()
circle canvas c paint = do Point2 x y <- (c^.center)&coordinates %%~ toJSVal
                           r          <- toJSVal $ c^.radius
                           void $ canvas ^.js4 ("drawCircle" :: MisoString) x y r paint
