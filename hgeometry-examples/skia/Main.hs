{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

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

type R = RealNumber 5

initialLayers :: Layers
initialLayers = Layers mempty (Layer "alpha" Visible) mempty



--------------------------------------------------------------------------------

data Model = Model { _canvas       :: (SkiaCanvas.Canvas R)
                   , _zoomConfig   :: ZoomConfig Double
                   , _points       :: IntMap.IntMap (Point 2 R :+ Attributes (Point 2 R))
                   , _polyLines    :: IntMap.IntMap (PolyLine' R :+ Attributes (PolyLine' R))
                   , _diagram      :: Maybe [Point 2 R]
                   , __layers      :: Layers
                   , _mode         :: Mode
                   , _strokeColor  :: Stroke
                   , _fillColor    :: Fill
                   } deriving (Eq,Show)
makeLenses ''Model

instance HasLayers Model where
  layers = _layers -- lens __layers (\m lrs -> m { __layers = lrs })

--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

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

data Action = Id
            | OnLoad
            | CanvasKitAction InitializeSkCanvasAction
            | CanvasResizeAction SkiaCanvas.CanvasResizeAction
            | CanvasAction SkiaCanvas.InternalCanvasAction
            | CanvasClicked
            | CanvasRightClicked
            | AddPoint
            | Draw
            | SetStrokeColor (Maybe Color)
            | SetFillColor   (Maybe Color)
            | NotifyError !MisoString
            | SwitchMode !Mode
            | ToggleLayerStatus !(Index Layers)


maybeToM     :: MonadError e m => e -> Maybe a -> m a
maybeToM msg = maybe (throwError msg) pure

handleDraw   :: Model -> ExceptT MisoString JSM Action
handleDraw m = do canvasKit <- maybeToM "Loading CanvasKit failed"  $ m^.canvas.canvasKitRef
                  surface'  <- maybeToM "Ackquiring surface failed" $ m^.canvas.surfaceRef
                  lift $ requestAnimationFrame canvasKit surface' (myDraw m)
                  pure Id

notifyOnError :: ExceptT MisoString JSM Action -> JSM Action
notifyOnError = fmap (\case
                         Left err  -> NotifyError err
                         Right act -> act
                     ) . runExceptT


onLoad   :: Model -> Effect Action Model
onLoad m = Effect m [ initializeCanvas
                    , initializeCanvasKit
                    ]
  where
    initializeCanvas :: Sub Action
    initializeCanvas = \sink -> acquireCanvasSize >>= liftIO . sink

    -- wrap the ackquireCanvasSize action in our Action type
    acquireCanvasSize :: JSM Action
    acquireCanvasSize = SkiaCanvas.acquireCanvasSize  theMainCanvasId <&> \case
      Left (SkiaCanvas.ErrorAction err) -> NotifyError err
      Right act                         -> CanvasResizeAction act

    initializeCanvasKit :: Sub Action
    initializeCanvasKit = mapSub CanvasKitAction $ initializeCanvasKitSub theMainCanvasId

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                     -> noEff m
    OnLoad                 -> onLoad m
    CanvasKitAction act    -> m&canvas %%~ flip SkiaCanvas.handleCanvasKitAction act
    CanvasResizeAction act -> m&canvas %%~ flip SkiaCanvas.handleCanvasResize act

    CanvasAction ca       -> m&canvas %%~ flip SkiaCanvas.handleInternalCanvasAction ca

    CanvasClicked         -> case m^.mode of
      PointMode -> addPoint
      _         -> noEff m

    CanvasRightClicked    -> noEff m

    AddPoint              -> addPoint
    Draw                  -> m <# notifyOnError (handleDraw m)

    ToggleLayerStatus lr  -> (m&layers.ix lr.status %~ toggleStatus)
                             <# pure Draw

    SetStrokeColor mc     -> noEff $ m&strokeColor %~ \cs ->
                               case mc of
                                 Nothing -> cs&strokeStatus       .~ InActive
                                 Just c  -> cs&strokeStatus       .~ Active
                                              &currentStrokeColor .~ c

    SetFillColor mc     -> noEff $ m&fillColor %~ \cs ->
                               case mc of
                                 Nothing -> cs&fillStatus       .~ InActive
                                 Just c  -> cs&fillStatus       .~ Active
                                              &currentFillColor .~ c
    NotifyError err -> noEff m -- TODO


    SwitchMode mode' -> noEff $ m&mode .~ mode'


  where
    addPoint = recomputeDiagram m' <# pure Draw
      where
        m' = case m^.canvas.mouseCoordinates of
               Nothing -> m
               Just p  -> let ats = PointAttributes $ toColoring (m^.strokeColor) (m^.fillColor)
                          in m&points %~ insertPoint (p :+ ats)

-- | Given a stroke and a fill, computes a coloring
--
-- if neither stroke or fill are set, we use the default (which is stroke using black)
toColoring     :: Stroke -> Fill -> Coloring
toColoring s f = case (s^.strokeStatus, f^.fillStatus) of
  (InActive, InActive) -> def -- this case is kind of weird
  (InActive, Active)   -> FillOnly   (f^.currentFillColor)
  (Active,   InActive) -> StrokeOnly (s^.currentStrokeColor)
  (Active,   Active)   -> StrokeAndFill (s^.currentStrokeColor) (f^.currentFillColor)


recomputeDiagram   :: Model -> Model
recomputeDiagram m
  | m^.points.to length <= 2  = m&diagram .~ Nothing
  | otherwise                 = let pts = NonEmpty.nonEmpty
                                          [ p :+ i | (i,p :+ _) <- IntMap.assocs (m^.points)]
                                in m&diagram .~ fmap voronoiVertices pts

insertPoint     :: p -> IntMap.IntMap p -> IntMap.IntMap p
insertPoint p m = let k = case IntMap.lookupMax m of
                            Nothing    -> 0
                            Just (i,_) -> succ i
                  in IntMap.insert k p m



--------------------------------------------------------------------------------

theMainCanvasId :: MisoString
theMainCanvasId = "mainCanvas"

viewModel   :: Model -> View Action
viewModel m =
    div_ []
         [ menuBar_ m
         , hero_ [ columns_ [ styleInline_ "height: calc(100vh - 102px)"]
                            [ leftPanel
                            , mainCanvas
                            , rightPanels
                            ]
                 ]
         , footer
         ]
  where
    leftPanel  = div_ [class_ "column is-narrow"]
                      [ menuButtons_ m
                      ]
    mainCanvas = column_ []
                      [ either CanvasAction id <$>
                        SkiaCanvas.skiaCanvas_
                                    theMainCanvasId
                                    [ styleM_ [ "width"      =: "100%"
                                              , "height"     =: "100%"
                                              , "min-width"  =: (w <> "px" :: MisoString)
                                              , "min-height" =: (h <> "px" :: MisoString)
                                              ]
                                    , onClick AddPoint
                                    ]
                      ]

    rightPanels = div_ [ class_ "column is-2"]
                       [ overviewPanel
                       , layersPanel
                       ]
    overviewPanel = panel_ [ styleInline_ "height: 60%"]
                           [ text "Model"
                           ]
                           [text . ms . show $ m
                           -- , message_ Nothing        [] [text "foo"]
                           -- , message_ (Just Warning) [] [text "warning :)"]
                           ]

    layersPanel = panel_ [ styleInline_ "height: 35%"

                         ]
                         [text "Layers"]
                         (map layer_ $ m^..layers.to theLayers.traverse)

    layer_   :: Layer -> View Action
    layer_ l = label_ [class_ "panel-block"]
                      [ input_ [ type_ "checkbox"
                               , name_    $ l^.name
                               , checked_ $ l^.status == Visible
                               , onClick  $ ToggleLayerStatus (l^.name)
                               ]
                      , text $ l^.name
                      ]


    footer = footer_ [ class_ "navbar is-fixed-bottom"]
                     [ navBarEnd_ [ p_ [class_ "navbar-item"]
                                       [ icon "fas fa-mouse-pointer" []
                                       , text $ case over coordinates ms <$>
                                                     m^.canvas.mouseCoordinates of
                                           Nothing           -> "-"
                                           Just (Point2 x y) -> " (" <> x <> ", " <> y <> ")"
                                       ]
                                  ]
                     ]

    Vector2 w h = ms <$> m^.canvas.dimensions



menuBar_   :: Model -> View Action
menuBar_ _ = navBar_

  --   canvasBody = [ g_ [] [ draw v [ fill_ "red"
  --                                 ]
  --                        ]
  --                | v <- m^..diagram.traverse.traverse ]
  --             <> [ g_ [] [ draw p [ fill_ "black"
  --                                 ]
  --                        , textAt p [] (ms i)
  --                        ]
  --                | (i,p) <- m^..points.ifolded.withIndex ]
  --             -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

--------------------------------------------------------------------------------

main :: IO ()
main = Run.runWith Options.jsAddleOptions 8080 $
         startApp $
            App { model         = initialModel
                , update        = flip updateModel
                , view          = viewModel
                , subs          = mempty
                , events        = SkiaCanvas.withCanvasEvents defaultEvents
                , initialAction = OnLoad
                , mountPoint    = Nothing
                , logLevel      = Off
                }



-- textAt                    :: ToMisoString r
--                           => Point 2 r
--                           -> [Attribute action] -> MisoString -> View action
-- textAt (Point2 x y) ats t = text_ ([ x_ $ ms x
--                                   , y_ $ ms y
--                                   ] <> ats
--                                   ) [text t]


--------------------------------------------------------------------------------
-- * The Navbar

navBar_ :: View Action
navBar_ = let theMainMenuId = "theMainMenuId"
          in
    nav_ [ class_ "navbar is-fixed-top"
         , textProp "role"       "navigation"
         , textProp "aria-label" "main navigation"
         ]
         [ navBarBurger_  theMainMenuId
                          [navBarBurgerItem_ ] -- not sure why we need these?
         , navBarMenu_ theMainMenuId
             [ navBarStart_ [ navBarItemA_ [onClick Draw ]
                                           [ text "foo" ]
                            , navBarSubMenu_ [ navBarItemA_ [] [text "bar"]]
                                             [ navBarItemA_ [] [text "child 1"]
                                             , navBarDivider_
                                             , navBarSelectedItemA_ [] [text "child 2"]
                                             ]
                            ]
             ]
         ]




navBarBurgerItem_ :: View action
navBarBurgerItem_ = span_ [boolProp "aria-hidden" True] []

navBarBrand_ :: [View action] -> View action
navBarBrand_ = div_ [class_ "navbar-brand"]

navBarBurger_           :: MisoString -> [View action] -> View action
navBarBurger_ theMenuId = a_ [class_ "navbar-burger"
                             , textProp "role"          "button"
                             , textProp "aria-label"    "menu"
                             , boolProp "aria-expanded" False
                             , textProp "data-target"   theMenuId
                             ]

navBarItemA_     :: [Attribute action] -> [View action] -> View action
navBarItemA_ ats = a_ ([class_ "navbar-item"] <> ats)


navBarSelectedItemA_     :: [Attribute action] ->  [View action] -> View action
navBarSelectedItemA_ ats = a_ ([class_ "navbar-item is-selected"] <> ats)


navBarMenu_       :: MisoString -> [View action] -> View action
navBarMenu_ theId = div_ [ class_ "navbar-menu"
                         , id_    theId
                         ]

navBarStart_ :: [View action] -> View action
navBarStart_ = div_ [class_ "navbar-start"]

    -- <nav class="navbar is-fixed-top" role="navigation" aria-label="main navigation">
    --   <div class="navbar-brand">
    --     <a class="navbar-item" href="https://bulma.io">
    --       thebrand
    --     </a>

    --     <a role="button" class="navbar-burger" aria-label="menu" aria-expanded="false" data-target="navbarBasicExample">
    --       <span aria-hidden="true"></span>
    --       <span aria-hidden="true"></span>
    --       <span aria-hidden="true"></span>
    --       <span aria-hidden="true"></span>
    --     </a>
    --   </div>

    --   <div id="navbarBasicExample" class="navbar-menu">
    --     <div class="navbar-start">
    --       <a class="navbar-item">
    --         Home
    --       </a>

    --       <a class="navbar-item">
    --         Documentation
    --       </a>




navBarSubMenu_             :: [View action] -- ^ the items to display for this item
                           -> [View action] -- ^ the children
                           -> View action
navBarSubMenu_ theItem chs =
    div_ [class_ "navbar-item has-dropdown is-hoverable"]
         (theItem <>
         [ div_ [class_ "navbar-dropdown"]
                chs
         ])

navBarDivider_ = hr_ [class_ "navbar-divider"]

navBarEnd_ = div_ [class_ "navbar-end"]

--------------------------------------------------------------------------------
-- * The Right Panel

panel_                 :: [Attribute action]
                       -> [View action] -> [View action] -> View action
panel_ ats heading chs =
    nav_ ([class_ "panel"] <> ats)
         ([ p_ [class_ "panel-heading"] heading
          , div_ [ class_ "container"
                 , styleM_ [ "height" =: "100%"
                           ]
                 ]
                 chs
          ]
         )

panelBlock_ = div_ [class_ "panel-block"]

panelTabs_ = p_ [class_ "panel-tabs"]


panelBlockActiveA_ = a_ [class_ "panel-block"]




--------------------------------------------------------------------------------
-- | Renders a message
message_            :: Maybe BulmaColor -> [Attribute action] -> [View action] -> View action
message_ mc ats bdy = article_ ([class_ $ "message" `withColor` mc] <> ats)
                               [div_ [class_ "meddage-body"]
                                     bdy
                               ]
  where
    withColor m = \case
      Nothing -> m
      Just c  -> m <> " " <> colorClass c


--------------------------------------------------------------------------------


myDraw                       :: Model -> CanvasKit -> SkCanvasRef -> JSM ()
myDraw m canvasKit canvasRef = do clear canvasKit canvasRef
                                  strokeOnly <- mkPaintStyle canvasKit CanvasKit.StrokeOnly
                                  myColor <- mkColor4f canvasKit (fromRGB24 192 40  27)
                                  withPaint canvasKit $ \paint -> do
                                    forM_ (m^.points) $ \p ->
                                      Render.point (m^.canvas) canvasRef p paint
                                    setAntiAlias paint True
                                    setColor paint myColor
                                    setStyle paint strokeOnly
                                    case NonEmpty.nonEmpty $ m^..points.traverse of
                                      Just pts@(_ :| _ : _) -> do
                                        let poly :: PolyLine (Point 2 R)
                                            poly = polyLineFromPoints $ (^.core) <$> pts
                                        Render.polyLine (m^.canvas) canvasRef poly paint
                                      _                     -> pure ()
                                  pure ()

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


data Selected = NotSelected | Selected
  deriving (Show,Read,Eq,Ord)


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




--------------------------------------------------------------------------------

hero_ chs = section_ [ class_ "hero"
                     , styleInline_ "height: calc(100vh - 92px)"
                     ]
                     [ div_ [ class_ "hero-body"
                            , styleM_ [ "padding-top"    =: "1rem"
                                      , "padding-bottom" =: "1rem"
                                      ]
                            ]
                            chs
                     ]
