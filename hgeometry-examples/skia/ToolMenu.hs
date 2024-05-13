{-# LANGUAGE OverloadedStrings          #-}
module ToolMenu
  ( menuButtons_
  , menuButton_
  ) where

import           Action
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


menuButtons_   :: Model -> View Action
menuButtons_ m = aside_ [class_ "menu"]
                        [ menuList_ [ selectButton
                                    , panButton
                                    ]
                        , zoomButtons_ m
                        , colorButtons
                        , toolButtons_ m
                        ]

  where
    selectButton = menuModeButton_ m SelectMode  "fas fa-mouse-pointer"
                                   [ title_ "Select" ]
    panButton    = menuModeButton_ m PanMode "fas fa-hand-pointer"
                                   [title_ "Pan"]

    colorButtons = menuList_ [ strokeButton
                             , fillButton
                             ]

    strokeButton = menuButton_  "fas fa-paint-brush"
                                [ title_ "Stroke color"
                                , styleM_ ["color" =: rgba (m^.strokeColor.currentStrokeColor)]
                                ]
                                (m^.strokeColor.strokeStatus)
                                NotSelected
                                [ onClick $ SetStrokeColor Nothing
                                ]
    fillButton   = menuButton_  "fas fa-fill"
                                [ title_ "Fill color"
                                , styleM_ ["color" =: rgba (m^.fillColor.currentFillColor)]
                                ]
                                (m^.fillColor.fillStatus)
                                NotSelected
                                [ onClick $ SetFillColor Nothing -- todo
                                ]

toolButtons_   :: Model -> View Action
toolButtons_ m = menuList_ [ pointButton
                           , penButton
                           , lineButton
                           , polyLineButton
                           , polygonButton
                           , rectangleButton
                           , circleButton
                           , textButton
                           , mathButton
                           ]
  where
    pointButton     = menuButton' PointMode "fas fa-circle"
                                  [title_ "Point"]
    penButton       = menuButton' PenMode "fas fa-pen"
                                  [title_ "Pen"]
    lineButton      = menuButton' LineMode  "fas fa-slash"
                                  [title_ "Line"]

    polyLineButton  = menuButton' PolyLineMode "fas fa-wave-square"
                                  [title_ "Polyline"]
    polygonButton   = menuButton' PolygonMode "fas fa-draw-polygon"
                                  [title_ "Polygon"]
    rectangleButton = menuButton' RectangleMode "far fa-square"
                                  [title_ "Rectangle"]
    circleButton    = menuButton' CircleMode "far fa-circle"
                                  [title_ "Circle"]
    textButton      = menuButton' TextMode "fas fa-font"
                                  [title_ "Text"]
    mathButton      = menuButton' MathMode "fas fa-square-root-alt"
                                  [title_ "Math Text"]

    menuButton' = menuModeButton_ m

menuModeButton_   :: Model -> Mode -> MisoString -> [Attribute Action] -> View Action
menuModeButton_ m mode' i ats = menuButton_ i
                                           ats
                                           (if m^.mode == mode' then Active else InActive)
                                           (if m^.mode == mode' then Selected else NotSelected)
                                           [ onClick $ SwitchMode mode'
                                           ]

menuList_ :: [View action] -> View action
menuList_ = ul_ [class_ "menu-list"]


-- | Renders a menu button
menuButton_                                 :: MisoString -- ^ icon to use
                                            -> [Attribute action] -- ^ attributes of the icon
                                            -> Status -- ^ whether the button is active or not
                                            -> Selected -- ^ whether the button is selected or not
                                            -> [Attribute action] -- ^ attributes of the button
                                            -> View action
menuButton_ i ats status selected buttonAts =
    li_ []
        [ button_ ( [ class_ "button is-medium"
                    , class_ $ case selected of
                                 NotSelected -> ""
                                 Selected    -> "is-selected is-primary"
                    , class_ $ case status of
                                 InActive   -> ""
                                 Active     -> "is-active"
                    ]
                  <> buttonAts
                  )
                  [ icon i ([styleInline_ "pointer-events: none;"] <> ats)
                  ] -- we need the style-Inline to make sure we can click the actual button.
        ]
  -- where
  --   withActive = case

  --     | isActive  = (<> " is-active")
  --              | otherwise = id

zoomButtons_ m = menuList_ [ menuButton_ "fas fa-plus-square"
                                         [ title_ "Zoom in"
                                         ]
                                         InActive NotSelected
                                         []

                           , menuButton_ "fas fa-equals"
                                         [ title_ "Zoom 1:1"
                                         ]
                                         (if m^.zoomConfig.currentLevel == 1
                                           then Active else InActive)
                                         NotSelected
                                         []
                           , menuButton_ "fas fa-minus-square"
                                         [ title_ "Zoom out"
                                         ]
                                         InActive NotSelected
                                         []
                           ]
