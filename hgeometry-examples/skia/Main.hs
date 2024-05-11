{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

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
import           GHC.TypeNats
import           GHCJS.Marshal
import           GHCJS.Types
import           HGeometry.Ext
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Vector
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
import qualified SkiaCanvas
import           SkiaCanvas (mouseCoordinates, dimensions, canvasKitRef, surfaceRef)
import           SkiaCanvas.CanvasKit
import qualified SkiaCanvas.CanvasKit as CanvasKit
import qualified SkiaCanvas.Render as Render
import           StrokeAndFill

--------------------------------------------------------------------------------

type R = RealNumber 5

initialLayers :: Layers
initialLayers = Layers [] (Layer "alpha" Visible) [ Layer "beta" Hidden
                                                  , Layer "foo" Visible
                                                  ]

--------------------------------------------------------------------------------


data Model = Model { _canvas       :: (SkiaCanvas.Canvas R)
                   , _points       :: IntMap.IntMap (Point 2 R)
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
                     , _points      = mempty
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
            -- | ToggleLayerStatus (Lens' Model Status)


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
    -- SetCanvasSize v       -> noEff $ m&canvas.dimensions   .~ v
    CanvasAction ca       -> m&canvas %%~ flip SkiaCanvas.handleInternalCanvasAction ca

    CanvasClicked         -> case m^.mode of
      PointMode -> addPoint
      _         -> noEff m

    CanvasRightClicked    -> noEff m

    AddPoint              -> addPoint
    Draw                  -> m <# notifyOnError (handleDraw m)
    -- ToggleLayerStatus lr  -> noEff $ m&lr %~ toggleStatus
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
  where
    addPoint = recomputeDiagram m' <# pure Draw
      where
        m' = case m^.canvas.mouseCoordinates of
               Nothing -> m
               Just p  -> m&points %~ insertPoint p


recomputeDiagram   :: Model -> Model
recomputeDiagram m
  | m^.points.to length <= 2  = m&diagram .~ Nothing
  | otherwise                 = let pts = NonEmpty.nonEmpty
                                          [ p :+ i | (i,p) <- IntMap.assocs (m^.points)]
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
                         (map layer_ $ m^.layers.to allLayers)

    layer_   :: Layer -> View action
    layer_ l = label_ [class_ "panel-block"]
                      [ input_ [ type_ "checkbox"
                               , name_    $ l^.name
                               , checked_ $ l^.status == Visible
                               -- , onClick  $ ToggleLayer l
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
                                  strokeOnly <- mkPaintStyle canvasKit StrokeOnly
                                  myColor <- mkColor canvasKit (RGB 192 40  27, Opaque)
                                  withPaint canvasKit $ \paint -> do
                                    forM_ (m^.points) $ \p ->
                                      Render.point (m^.canvas) canvasRef p paint
                                    setAntiAlias paint True
                                    setColor paint myColor
                                    setStyle paint strokeOnly
                                    case NonEmpty.nonEmpty $ m^..points.traverse of
                                      Just pts@(_ :| _ : _) -> do
                                        let poly :: PolyLine (Point 2 R)
                                            poly = polyLineFromPoints pts
                                        Render.polyLine (m^.canvas) canvasRef poly paint
                                      _                     -> pure ()
                                  pure ()

--------------------------------------------------------------------------------

menuButtons_   :: Model -> View Action
menuButtons_ m = aside_ [class_ "menu"]
                        [ menuList_ [ selectButton
                                    , panButton
                                    ]
                        , zoomButtons_
                        , colorButtons
                        , toolButtons_
                        ]

  where
    selectButton = menuButton_  "fas fa-mouse-pointer"
                                [ title_ "Select"
                                ]
                                Nothing
                                []
    panButton = menuButton_  "fas fa-hand-pointer"
                             [title_ "Pan"
                             ]
                             Nothing
                             []

    colorButtons = menuList_ [ strokeButton
                             , fillButton
                             ]

    strokeButton = menuButton_  "fas fa-paint-brush"
                                [ title_ "Stroke color"
                                , styleM_ ["color" =: (m^.strokeColor.currentStrokeColor)]
                                ]
                                (m^?strokeColor.strokeStatus._Active)
                                [ onClick $ SetStrokeColor Nothing
                                ]
    fillButton   = menuButton_  "fas fa-fill"
                                [ title_ "Fill color"
                                , styleM_ ["color" =: (m^.fillColor.currentFillColor)]
                                ]
                                (m^?fillColor.fillStatus._Active)
                                [ onClick $ SetFillColor Nothing -- todo
                                ]

toolButtons_ = menuList_ [ pointButton
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
    pointButton     = menuButton_ "fas fa-circle"
                        [title_ "Point"]
                        Nothing
                        []
    penButton       = menuButton_ "fas fa-pen"
                        [title_ "Pen"]
                        Nothing
                        []
    lineButton      = menuButton_ "fas fa-slash"
                        [title_ "Line"]
                        Nothing
                        []
    polyLineButton  = menuButton_ "fas fa-wave-square"
                        [title_ "Polyline"]
                        Nothing
                        []
    polygonButton   = menuButton_ "fas fa-draw-polygon"
                        [title_ "Polygon"]
                        Nothing
                        []
    rectangleButton = menuButton_ "far fa-square"
                        [title_ "Rectangle"]
                        Nothing
                        []
    circleButton    = menuButton_ "far fa-circle"
                        [title_ "Circle"]
                        Nothing
                        []
    textButton      = menuButton_ "fas fa-font"
                        [title_ "Text"]
                        Nothing
                        []
    mathButton      = menuButton_ "fas fa-square-root-alt"
                        [title_ "Math Text"]
                        Nothing
                        []


menuList_ :: [View action] -> View action
menuList_ = ul_ [class_ "menu-list"]

menuButton_                         :: MisoString -> [Attribute action]
                                    -> Maybe a -> [Attribute action]
                                    -> View action
menuButton_ i ats mActive buttonAts =
    li_ []
        [ button_ ( [class_ $ withActive "button is-medium"]
                  <> buttonAts
                  )
                  [ icon i ats
                  ]
        ]
  where
    withActive = case mActive of
                   Nothing -> id
                   Just _  -> (<> " is-active")


zoomButtons_ = menuList_ [ menuButton_ "fas fa-plus-square"
                                       [ title_ "Zoom in"
                                       ]
                                       Nothing
                                       []

                        , menuButton_ "fas fa-equals"
                                       [ title_ "Zoom 1:1"
                                       ]
                                       Nothing
                                       []
                        , menuButton_ "fas fa-minus-square"
                                       [ title_ "Zoom out"
                                       ]
                                       Nothing
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
