{-# LANGUAGE OverloadedStrings          #-}
module ToolMenu
  ( menuButtons_
  , menuButton_

  , colorModal_
  ) where

import Action
import Color
import Control.Lens hiding (view, element)
import HGeometry.Viewport (currentLevel)
import Miso
import Miso.Bulma.Generic
import Miso.Bulma.Modal
import Miso.String (MisoString)
import Model
import Modes
import StrokeAndFill
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
                                [ onClick $ StrokeModalAction ToggleModalStatus
                                ]
    fillButton   = menuButton_  "fas fa-fill"
                                [ title_ "Fill color"
                                , styleM_ ["color" =: rgba (m^.fillColor.currentFillColor)]
                                ]
                                (m^.fillColor.fillStatus)
                                NotSelected
                                [ onClick $ FillModalAction ToggleModalStatus
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

    polyLineButton  = menuButton' (PolyLineMode Nothing) "fas fa-wave-square"
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

menuModeButton_   :: Model
                  -> Mode
                  -> MisoString -> [Attribute Action] -> View Action
menuModeButton_ m mode' i ats = menuButton_ i
                                           ats
                                           (if (m^.mode) `matches` mode' then Active else InActive)
                                           (if (m^.mode) `matches` mode' then Selected else NotSelected)
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

zoomButtons_   :: Model -> View Action
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



--------------------------------------------------------------------------------

colorModal_                   :: Status -> (ModalAction -> Action) -> View Action
colorModal_ status liftAction = either liftAction id <$>
    modal_ status
           [ text "woei"
           ]
  -- where
  --   content :: [View Action]
  --   content =
  --     [ text "woei"
  --     ]

-- <div class="modal">
--   <div class="modal-background"></div>
--   <div class="modal-content">
--     <!-- Any other Bulma elements you want -->
--   </div>
--   <button class="modal-close is-large" aria-label="close"></button>
-- </div>
