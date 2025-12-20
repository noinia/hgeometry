{-# LANGUAGE OverloadedStrings          #-}
module ToolMenu
  ( menuButtons_
  , menuButton_

  , colorModal_
  ) where

import Action
import Color
import Control.Lens hiding (view, element)
import Data.Default
import HGeometry.Viewport (currentLevel)
import Miso hiding (style_)
import Miso.Bulma.Generic
import Miso.Bulma.Modal
import Miso.String (MisoString)
import Miso.Style ((=:), style_, styleInline_)
import Model
import Modes
import SelectMode
import StrokeAndFill

--------------------------------------------------------------------------------


menuButtons_   :: Model -> View Action
menuButtons_ m =
    aside_ [class_ "menu"]
           [ menuList_ [ selectButton
                       , panButton
                       ]
           , zoomButtons_ m
           , colorButtons
           , toolButtons_ m
           ]
  where
    selectButton = menuModeButton_ m (SelectMode def)  "fas fa-mouse-pointer"
                                   [ title_ "Select" ]
    panButton    = menuModeButton_ m PanMode "fas fa-hand-pointer"
                                   [title_ "Pan"]

    colorButtons = menuList_ [ strokeButton
                             , fillButton
                             ]

    strokeButton = menuButton_  "fas fa-paint-brush"
                                [ style_ ["color" =: rgba (m^.stroke.color)]
                                ]
                                (m^.stroke.status)
                                NotSelected
                                [ onClick $ StrokeAction ToggleModal
                                , title_ "Stroke color"
                                ]
    fillButton   = menuButton_  "fas fa-fill"
                                [ style_ ["color" =: rgba (m^.fill.color)]
                                ]
                                (m^.fill.status)
                                NotSelected
                                [ onClick $ FillAction ToggleModal
                                , title_ "Fill color"
                                ]


toolButtons_   :: Model -> View Action
toolButtons_ m =
    menuList_ [ pointButton
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

    polyLineButton  = menuButton' (PolyLineMode def) "fas fa-wave-square"
                                  [title_ "Polyline"]
    polygonButton   = menuButton' (PolygonMode def) "fas fa-draw-polygon"
                                  [title_ "Polygon"]
    rectangleButton = menuButton' (RectangleMode def) "far fa-square"
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
menuModeButton_ m mode' i ats =
    menuButton_ i
                [] -- no icon ats
                (if (m^.mode) `matches` mode' then Active else InActive)
                (if (m^.mode) `matches` mode' then Selected else NotSelected)
                ((onClick $ SwitchMode mode')
                : ats
                )

menuList_ :: [View action] -> View action
menuList_ = ul_ [class_ "menu-list"]


-- | Renders a menu button
menuButton_                                 :: MisoString -- ^ icon to use
                                            -> [Attribute action] -- ^ attributes of the icon
                                            -> Status -- ^ whether the button is active or not
                                            -> Selected -- ^ whether the button is selected or not
                                            -> [Attribute action] -- ^ attributes of the button
                                            -> View action
menuButton_ i ats status' selected buttonAts =
    li_ []
        [ button_ ( [ class_ "button is-medium"
                    , class_ $ case selected of
                                 NotSelected -> ""
                                 Selected    -> "is-selected is-primary"
                    , class_ $ case status' of
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
zoomButtons_ m =
    menuList_ [ menuButton_ "fas fa-plus-square"
                           [ ]
                           InActive NotSelected
                           [ title_ "Zoom in"
                           ]

             , menuButton_ "fas fa-equals"
                           [ ]
                           (if m^.zoomConfig.currentLevel == 1
                             then Active else InActive)
                           NotSelected
                           [ title_ "Zoom 1:1"
                           ]
             , menuButton_ "fas fa-minus-square"
                           [ ]
                           InActive NotSelected
                           [ title_ "Zoom out"
                           ]
             ]

--------------------------------------------------------------------------------

-- | Renders the modal to select the stroke/fill color
colorModal_                           :: Status -- ^ Status of the modal
                                      -> StrokeFill
                                      -> MisoString -- ^ title
                                      -> View ColorAction
colorModal_ modalStatus current title = either (const ToggleModal) id <$>
    modalCard_ modalStatus title bodyContent footerContent
  where
    theIcon = case current^.status of
                InActive -> "fas fa-times"
                Active   -> "fas fa-check"

    bodyContent =
      [ button_ [ class_ "button"
                , class_ $ case current^.status of
                             InActive -> ""
                             Active   -> "is-active"
                , style_ ["background-color" =: rgba (current^.color) ]
                , onClick $ ToggleColor
                ]
                [ icon theIcon []
                , span_ []
                        [ text $ case current^.status of
                                   InActive -> "Enable"
                                   Active   -> "Disable"
                        ]
                ]
      , div_ [class_ "buttons"]
             (foldMap (\c -> [button_ [ class_ "button"
                                      , style_ ["background-color" =: rgba c]
                                      , onClick $ SetColor c
                                      ]
                                      [ ]
                             ]
                      ) colorPresets
             )
      ]
    footerContent = []
