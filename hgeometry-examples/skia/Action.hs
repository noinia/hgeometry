module Action
  ( Action(..)
  , ColorAction(..)
  ) where

import           Color
import           Control.Lens hiding (view, element)
import           HGeometry.Miso.OrphanInstances ()
import           Layers
import           Miso.String (MisoString)
import           Model
import           Modes
import           RectangleMode (Rectangle')
import qualified SkiaCanvas
import           SkiaCanvas.CanvasKit hiding (Style(..))

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

            | ReDraw
            | StoreCached {-# UNPACK #-}!SkPictureRef

            -- | SetStrokeColor (Maybe Color)
            -- | SetFillColor   (Maybe Color)
            | NotifyError !MisoString
            | SwitchMode !Mode
            | ToggleLayerStatus !(Index Layers)

            | StrokeAction !ColorAction
            | FillAction !ColorAction

            | AddLayer


            | ComputeSelection (Rectangle' R)

            | SaveSkpFile
            | LoadSkpFile

            | TriangulateSelectedPolygon -- triangulates the selected polygon




-- | Actions one can do with the stroke or fill color
data ColorAction = ToggleModal
                 | ToggleColor
                 | SetColor !Color
                 deriving (Show,Eq)
