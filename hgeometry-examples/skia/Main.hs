{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main
  ( main
  , mainJSM
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
-- import           Data.Maybe (maybe)
import qualified Data.Sequence as Seq
import           GHC.TypeNats
import           GHCJS.Marshal
import           GHCJS.Types
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Interval
import           HGeometry.Miso.Event.Extra
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Simple
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
import           Miso.Bulma.Modal
import           Miso.Bulma.NavBar
import           Miso.Bulma.Panel
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Model
import           Modes
import           Options
import           PolyLineMode
import           RectangleMode
import           SelectMode
import qualified SkiaCanvas
import           SkiaCanvas (mouseCoordinates, dimensions, canvasKitRef, surfaceRef)
import qualified SkiaCanvas.CanvasKit as CanvasKit
import           SkiaCanvas.CanvasKit hiding (Style(..))
import qualified SkiaCanvas.CanvasKit.Core as Core
import qualified SkiaCanvas.Render as Render
import           StrokeAndFill
import           ToolMenu

import           Debug.Trace
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------



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

    CanvasAction ca       -> do
                               m' <- m&canvas %%~ flip SkiaCanvas.handleInternalCanvasAction ca
                               -- if we moved the mouse, also redraw
                               case ca of
                                 SkiaCanvas.MouseMove _ -> () <# pure Draw
                                 _                      -> noEff () -- otherwise just return
                               pure m'

    CanvasClicked       -> case m^.mode of
        SelectMode{}           ->
            do m' <- m&mode._SelectMode %%~ updateSelection ComputeSelection
                                                            (m^.canvas.mouseCoordinates)
               () <# pure Draw
               return m'
        PointMode              -> addPoint
        PenMode                -> noEff m
        PolyLineMode{}         -> (m&mode._PolyLineMode.currentPoly %~ extend)
                                  <# pure Draw
        RectangleMode mData -> case mData^.currentRect of
          Just _  -> noEff m
          Nothing -> (m&mode._RectangleMode %~ startRectangleWith (m^.canvas.mouseCoordinates))
                     <# pure Draw
        _                      -> noEff m

    CanvasRightClicked -> case m^.mode of
        PenMode             -> noEff m
        PolyLineMode mData  -> addPoly mData
        RectangleMode mData -> addRect mData
        _                   -> noEff m


    Draw                  -> m <# notifyOnError (handleDraw m)

    ToggleLayerStatus lr  -> (m&layers.ix lr.layerStatus %~ toggleLayerStatus)
                             <# pure Draw

    -- SetStrokeColor mc     -> noEff $ m&stroke %~ \cs ->
    --                            case mc of
    --                              Nothing -> cs&status       .~ InActive
    --                              Just c  -> cs&status       .~ Active
    --                                           &color .~ c

    -- SetFillColor mc     -> noEff $ m&fillColor %~ \cs ->
    --                            case mc of
    --                              Nothing -> cs&fillStatus       .~ InActive
    --                              Just c  -> cs&fillStatus       .~ Active
    --                                           &currentFillColor .~ c

    NotifyError err -> noEff m -- TODO


    SwitchMode mode' -> noEff $ m&mode .~ mode'

    -- allow toggling the stroke and fill modals base on the status and fillStatus
    StrokeAction act -> noEff $ m&currentModal .~ cm
                                 &stroke       %~ maybe id const ms
      where
        (cm, ms) = handleColorAction (m^.stroke) (m^.currentModal) StrokeModal act
    FillAction act   -> noEff $ m&currentModal .~ cm
                                 &fill         %~ maybe id const mf
      where
        (cm, mf) = handleColorAction (m^.fill) (m^.currentModal) FillModal act


    AddLayer         -> noEff $ m&layers %~ addLayer

    ComputeSelection rng -> noEff $ m -- TODO

  where
    extend = extendWith (m^.canvas.mouseCoordinates)

    -- startMouseDown = case m^.mode of
    --     PolyLineMode -> noEff m
    --     PenMode      -> noEff $ m&currentPoly .~ extend Nothing
    -- mouseMove      = case m^.mode of
    --     PolyLineMode -> noEff m
    --     PenMode      -> noEff $ m&currentPoly %~ \mp -> case mp of
    --                                                       Nothing -> Nothing
    --                                                       Just _  -> extend mp
    -- stopMouseDown  = case m^.mode of
    --     PolyLineMode -> noEff m
    --     PenMode      -> addPoly

    addPoint = recomputeDiagram m' <# pure Draw
      where
        m' = case m^.canvas.mouseCoordinates of
               Nothing -> m
               Just p  -> let ats = PointAttributes $ toColoring (m^.stroke) (m^.fill)
                          in m&points %~ insert (p :+ ats)

    addPoly mData = m' <# pure Draw
      where
        m' = m&polyLines                      %~ addPoly'
              &mode._PolyLineMode.currentPoly .~ Nothing

        addPoly' = case extend (mData^.currentPoly) of
          Just (PartialPolyLine p) -> let ats = PolyLineAttributes (m^.stroke.color)
                                                                   Normal
                                      in insert (p :+ ats)
          _                        -> id

    addRect mData = m' <# pure Draw
      where
        m' = m&rectangles %~ addRect'
              &mode._RectangleMode.currentRect .~ Nothing

        addRect' = case asRectangleWith (m^.canvas.mouseCoordinates) mData of
                     Nothing -> id
                     Just r  -> insert (r :+ ats)

        ats = RectangleAttributes (toColoring (m^.stroke) (m^.fill)) Normal


-- | Updates setting a color
handleColorAction                         :: StrokeFill
                                          -> Maybe Modal -- ^ modal status
                                          -> Modal -- ^ the modal to set (if any)
                                          -> ColorAction
                                          -> ( Maybe Modal
                                             , Maybe StrokeFill
                                             )
handleColorAction sf modalStatus theModal = \case
    ToggleModal -> ( case modalStatus of
                            Nothing                -> Just theModal
                            Just m | m == theModal -> Nothing
                                   | otherwise     -> modalStatus
                   , Nothing -- no change to the StrokeAndFill
                   )
    ToggleColor -> ( Nothing -- we've changed the color status, so close the modal
                   , Just $ sf&status %~ toggleStatus
                   )
    SetColor c  -> ( Nothing -- we've  changed the color, so close the modal
                   , Just $ sf&color .~ c
                   )


-- | Handles the given toggleModal action
toggleModal                  :: Modal -> Maybe Modal -> ModalAction -> Maybe Modal
toggleModal theModal current = \case
  ToggleModalStatus -> case current of
                         Nothing                -> Just theModal
                         Just m | m == theModal -> Nothing
                         Just _                 -> current
                           -- this is kind of weird, since the modal that is currentlyActive
                           -- is not actually the modal that we're getting a ToggleModalAction
                           -- for


recomputeDiagram   :: Model -> Model
recomputeDiagram m
  | m^.points.to length <= 2  = m&diagram .~ Nothing
  | otherwise                 = let pts = NonEmpty.nonEmpty
                                          [ p :+ i | (i,p :+ _) <- IntMap.assocs (m^.points)]
                                in m&diagram .~ fmap voronoiVertices pts

-- | Helper to insert a new item into an IntMap
insert     :: p -> IntMap.IntMap p -> IntMap.IntMap p
insert p m = let k = case IntMap.lookupMax m of
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
                                    , onClick      CanvasClicked
                                    , onRightClick CanvasRightClicked
                                    ]
                      , StrokeAction <$>
                        colorModal_ (currentStatus (m^.currentModal) StrokeModal)
                                    (m^.stroke)
                                    "Stroke Color"

                      , FillAction <$>
                        colorModal_ (currentStatus (m^.currentModal) FillModal)
                                    (m^.fill)
                                    "Fill Color"
                      ]

    rightPanels = div_ [ class_ "column is-2"]
                       [ overviewPanel
                       , layersPanel m
                       ]
    overviewPanel = panel [ styleM_ [ "height"   =: "60%"
                                    , "overflow" =: "scroll"
                                    ]
                          ]
                          [ text "Model" ]
                          [ panelBlock
                              [text . ms . show $ m^.canvas ]
                          , panelBlock
                              [text . ms . show $ m^.zoomConfig ]
                          , panelBlock
                              [text . ms . show $ m^.zoomConfig ]
                          , panelBlock
                              [text . ms . show $ m^.mode ]
                          , panelBlock
                              [text . ms . show $ m^.points ]
                          , panelBlock
                              [text . ms . show $ m^.polyLines ]
                          , panelBlock
                              [text . ms . show $ m^.diagram ]
                          , panelBlock
                              [text . ms . show $ m^.stroke ]
                          , panelBlock
                              [text . ms . show $ m^.fill ]
                           -- , message_ Nothing        [] [text "foo"]
                           -- , message_ (Just Warning) [] [text "warning :)"]
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



-- | Our "main", full width/full height, hero section
hero_     :: [View action] -> View action
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

-- | Renders the Layers panel
layersPanel   :: Model -> View Action
layersPanel m =
      panel [ styleM_ [ "height"   =: "35%"
                      , "overflow" =: "scroll"
                      ]
            ]
            [text "Layers"]
            (  (map (layer_ InActive) $ m^..layers.beforeActive.traverse)
            <> [     layer_ Active    $ m^.layers.activeLayer]
            <> (map (layer_ InActive) $ m^..layers.afterActive.traverse)
            <>
            [ panelBlock
                [ button_ [class_ "button is-primary is-outlined is-fullwidth"
                          , onClick $ AddLayer
                          ]
                          [ "Add Layer" ]
                ]
            ])
  where
    layer_           :: Status -> Layer -> View Action
    layer_ status' l =
      label_ [ class_ "panel-block"
             , class_ $ case status' of
                 InActive -> ""
                 Active   -> "has-background-primary"
             ]
             [ input_ [ type_ "checkbox"
                      , name_    $ l^.name
                      , checked_ $ l^.layerStatus == Visible
                      , onClick  $ ToggleLayerStatus (l^.name)
                      ]
             , text $ l^.name
             ]



--------------------------------------------------------------------------------

main :: IO ()
main = Run.runWith Options.jsAddleOptions 8080 mainJSM

mainJSM :: JSM ()
mainJSM = startApp $
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
             [ navBarStart_ [ navBarSubMenu_ [ navBarItemA_ []
                                                            [text "File"]
                                             ]
                                             [ navBarItemA_ []
                                                            [text "Open"]
                                             , navBarDivider_
                                             , navBarSelectedItemA_ []
                                                                    [text "Save"]
                                             ]
                            , navBarSubMenu_ [ navBarItemA_ []
                                                            [text "Run"]
                                             ]
                                             [ navBarItemA_ []
                                                            [text "Convex Hull"]
                                             , navBarSelectedItemA_ []
                                                                    [text "Voronoi Diagram"]
                                             ]
                            , navBarItemA_ [onClick Draw ]
                                           [ text "Draw" ]
                            ]
             ]
         ]






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


--------------------------------------------------------------------------------
-- * The Right Panel



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
myDraw m canvasKit canvasRef = do
    -- some setup
    strokeOnly <- mkPaintStyle canvasKit CanvasKit.StrokeOnly
    fillOnly <- mkPaintStyle canvasKit CanvasKit.FillOnly
    -- clear the canvas
    clear canvasKit canvasRef
    -- render all polylines
    forM_ (m^.polyLines) $ \poly ->
      renderPoly strokeOnly (m^.canvas) poly canvasKit canvasRef
    -- render all rectangles
    forM_ (m^.rectangles) $ \rect ->
      renderRect strokeOnly fillOnly (m^.canvas) rect canvasKit canvasRef
      -- render all points
    forM_ (m^.points) $ \p ->
      renderPoint strokeOnly fillOnly (m^.canvas) p canvasKit canvasRef

    case m^.mode of
      PolyLineMode mData ->
        let current' = extendWith (m^.canvas.mouseCoordinates) (mData^.currentPoly)
        in renderPartialPolyLine strokeOnly
                        (m^.canvas)
                        current' canvasKit canvasRef
      RectangleMode mData -> forM_ (asRectangleWith (m^.canvas.mouseCoordinates) mData) $ \rect ->
                                let ats = RectangleAttributes def Normal
                                in renderRect strokeOnly fillOnly (m^.canvas) (rect :+ ats)
                                              canvasKit canvasRef
      SelectMode mData -> do renderSelectionRange
                             renderSelection
        where
          renderSelectionRange =
            forM_ (asRectangleWith (m^.canvas.mouseCoordinates) mData) $ \r ->
              let rect     = r :+ selectAttributes
              in renderRect strokeOnly fillOnly (m^.canvas) rect canvasKit canvasRef
          renderSelection = pure () -- TODO


      _                     -> pure ()
    -- render cursor
    case m^.canvas.mouseCoordinates of
      Nothing -> pure ()
      Just p  -> renderPoint strokeOnly fillOnly (m^.canvas) (p :+ cursorAttributes)
                             canvasKit canvasRef


cursorAttributes :: Attributes (Point 2 R)
cursorAttributes = def&coloring .~ StrokeAndFill red red
  where
    red = fromRGB24 192 40  27 -- darkish red

selectAttributes :: Attributes (Rectangle' R)
selectAttributes = def&coloring .~ StrokeAndFill darkishGrey (lightGrey&opacity .~ Alpha 0.1)

--------------------------------------------------------------------------------

renderPoint :: SkPaintStyle
            -> SkPaintStyle
            -> SkiaCanvas.Canvas R
            -> (Point 2 R :+ Attributes (Point 2 R))
            -> CanvasKit -> SkCanvasRef -> JSM ()
renderPoint strokeOnly fillOnly canvas' (p :+ ats) canvasKit skCanvasRef =
    renderColoring strokeOnly fillOnly canvas' render canvasKit skCanvasRef (ats^.coloring)
  where
    render paint = Render.point canvas' skCanvasRef p paint

renderColoring  :: SkPaintStyle
                -> SkPaintStyle
                -> SkiaCanvas.Canvas R
                -> (Core.SkPaintRef -> JSM ()) -- ^ the renderer
                -> CanvasKit -> SkCanvasRef
                -> Coloring
                -> JSM ()
renderColoring strokeOnly fillOnly canvas' render canvasKit skCanvasRef = \case
    StrokeOnly s      -> stroke' s
    FillOnly f        -> fill' f
    StrokeAndFill s f -> fill' f >> stroke' s
  where
    stroke' c = withColor' c canvasKit $ \paint ->
                 do setStyle paint strokeOnly
                    setAntiAlias paint True
                    render paint
    fill' c = withColor' c canvasKit $ \paint ->
                 do setStyle paint fillOnly
                    setAntiAlias paint True
                    render paint

renderRect :: SkPaintStyle
            -> SkPaintStyle
            -> SkiaCanvas.Canvas R
            -> (Rectangle' R :+ Attributes (Rectangle' R))
            -> CanvasKit -> SkCanvasRef -> JSM ()
renderRect strokeOnly fillOnly canvas' (r :+ ats) canvasKit skCanvasRef =
    renderColoring strokeOnly fillOnly canvas' render canvasKit skCanvasRef (ats^.coloring)
  where
    render paint = let poly :: SimplePolygon (Point 2 R)
                       poly = uncheckedFromCCWPoints $ corners r
                   in Render.simplePolygon canvas' skCanvasRef poly paint

renderPoly                             :: SkPaintStyle
                                       -> SkiaCanvas.Canvas R
                                       -> (PolyLine' R :+ Attributes (PolyLine' R))
                                       -> CanvasKit -> SkCanvasRef -> JSM ()
renderPoly strokeOnly canvas' (pl :+ ats) canvasKit skCanvasRef =
  withColor' (ats^.color) canvasKit $ \paint ->
    do setStyle paint strokeOnly
       setAntiAlias paint True
       Render.polyLine canvas' skCanvasRef pl paint

withColor'                    :: Color -> CanvasKit -> (Core.SkPaintRef -> JSM a) -> JSM a
withColor' c canvasKit render = withPaint canvasKit $ \paint -> do
    c' <- mkColor4f canvasKit c
    setColor paint c'
    render paint





renderPartialPolyLine :: SkPaintStyle
                      -> SkiaCanvas.Canvas R
                      -> Maybe (PartialPolyLine R)
                      -> CanvasKit
                      -> SkCanvasRef
                      -> JSM ()
renderPartialPolyLine strokeOnly canvas' partialPoly canvasKit skCanvasRef = case partialPoly of
    Just (PartialPolyLine pl) -> renderPoly strokeOnly canvas' (pl :+ ats) canvasKit skCanvasRef
    Just (StartPoint _)       -> pure ()
    Nothing                   -> pure ()
  where
    ats = def

--------------------------------------------------------------------------------