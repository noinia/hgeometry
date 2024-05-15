{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Model
  ( Model(Model)
  , canvas, zoomConfig, points, polyLines, diagram, _layers, mode, strokeColor, fillColor
  , initialModel

  , R

  , Selected(..)
  ) where

import           Attributes
import           Base
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
import           GHCJS.Marshal
import           GHCJS.Types
import           HGeometry.Ext
import           HGeometry.Interval
import           HGeometry.Miso.OrphanInstances ()
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


initialLayers :: Layers
initialLayers = Layers mempty (Layer "alpha" Visible) mempty

--------------------------------------------------------------------------------

data Model = Model { _canvas       :: (SkiaCanvas.Canvas R)
                   , _zoomConfig   :: ZoomConfig Double
                   , _mode         :: Mode
                   -- , _modeData     :: ModeData
                   , _points       :: IntMap.IntMap (Point 2 R :+ Attributes (Point 2 R))
                   , _polyLines    :: IntMap.IntMap (PolyLine' R :+ Attributes (PolyLine' R))
                   , _diagram      :: Maybe [Point 2 R]
                   , __layers      :: Layers
                   , _strokeColor  :: Stroke
                   , _fillColor    :: Fill
                   } deriving (Eq,Show)
makeLenses ''Model

instance HasLayers Model where
  layers = _layers -- lens __layers (\m lrs -> m { __layers = lrs })

--------------------------------------------------------------------------------


instance Default (Point 2 R :+ Int) where
  def = origin :+ 0

----------------------------------------

initialModel :: Model
initialModel = Model { _canvas      = SkiaCanvas.blankCanvas 1024 768
                     , _zoomConfig  = ZoomConfig (ClosedInterval 0.1 4) 1
                     , _points      = mempty
                     , _polyLines   = mempty
                     , _diagram     = Nothing
                     , __layers     = initialLayers
                     , _mode        = PointMode
                     , _strokeColor = defaultStroke
                     , _fillColor   = defaultFill
                     }


--------------------------------------------------------------------------------


data Selected = NotSelected | Selected
  deriving (Show,Read,Eq,Ord)
