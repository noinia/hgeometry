module Action
  ( Action(..)
  ) where

import           Attributes
import           Color
import           Control.Lens hiding (view, element)
import           Control.Monad (forM_)
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Colour.SRGB
import           Data.Default.Class
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           GHC.TypeNats
import           GHCJS.Marshal
import           GHCJS.Types
import           HGeometry.Ext
import           HGeometry.Interval
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Vector
import           HGeometry.Viewport (ZoomConfig(..), currentLevel, range)
import           HGeometry.VoronoiDiagram
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (jsg1, jsg2, jsf, js1, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Layers
import           Miso
import           Miso.Bulma.Color
import           Miso.Bulma.Columns
import           Miso.Bulma.Generic
import qualified Miso.Bulma.JSAddle as Run
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Model
import           Modes
import           Options
import           PolyLineMode
import qualified SkiaCanvas
import           SkiaCanvas (mouseCoordinates, dimensions, canvasKitRef, surfaceRef)
import qualified SkiaCanvas.CanvasKit as CanvasKit
import           SkiaCanvas.CanvasKit hiding (Style(..))
import qualified SkiaCanvas.Render as Render
import           StrokeAndFill


--------------------------------------------------------------------------------

data Action = Id
            | OnLoad
            | CanvasKitAction InitializeSkCanvasAction
            | CanvasResizeAction SkiaCanvas.CanvasResizeAction
            | CanvasAction SkiaCanvas.InternalCanvasAction
            | CanvasClicked
            | CanvasRightClicked
            -- | AddPoint
            | Draw
            | SetStrokeColor (Maybe Color)
            | SetFillColor   (Maybe Color)
            | NotifyError !MisoString
            | SwitchMode !Mode
            | ToggleLayerStatus !(Index Layers)
