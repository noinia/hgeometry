{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

import           Control.Lens hiding (view, element)
import           Control.Monad.IO.Class
import           Data.Default.Class
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           GHC.TypeNats
import           HGeometry.Ext
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram
import           Miso
import qualified Miso.Bulma.JSAddle as Run
-- import           Miso.Bulma.Panel
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Options
import qualified SkiaCanvas
import           SkiaCanvas (mouseCoordinates, dimensions)
import           Miso.Bulma.Generic
import           GHCJS.Types
import           GHCJS.Marshal
import           Language.Javascript.JSaddle.Object (jsg1, jsg2, jsf, js1, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import qualified Language.Javascript.JSaddle as JSAddle

--------------------------------------------------------------------------------
type R = RealNumber 5

data LayerStatus = Hidden | Visible
  deriving (Show,Eq)

toggleStatus = \case
  Hidden  -> Visible
  Visible -> Hidden

data Layer = Layer { _name :: MisoString
                   , _status :: LayerStatus
                   }
             deriving (Show,Eq)
makeClassy ''Layer

data Layers = Layers { _beforeActive :: [Layer] -- stored in reverse, so a zipper
                     , _activeLayer  :: Layer
                     , _afterActive  :: [Layer]
                     }
            deriving (Show,Eq)

allLayers                              :: Layers -> [Layer]
allLayers (Layers before active after) = reverse before <> [active] <> after

makeClassy ''Layers

initialLayers :: Layers
initialLayers = Layers [] (Layer "alpha" Visible) [ Layer "beta" Hidden
                                                  , Layer "foo" Visible
                                                  ]

--------------------------------------------------------------------------------
-- * The CanvasKit object

 -- ^ the CanvasKit object
newtype CanvasKit = MkCanvasKit JSVal
  deriving newtype (JS.MakeObject,JSAddle.ToJSVal)

instance Show CanvasKit where
  show _ = "CanvasKitObj"
instance Eq CanvasKit where
  a == b = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * Surface

 -- ^ the Surface object
newtype Surface = MkSurface JSVal
  deriving newtype (JS.MakeObject, JSAddle.ToJSVal)

instance Show Surface where
  show _ = "SurfaceObj"
instance Eq Surface where
  a == b = True
  -- we should only have one
--------------------------------------------------------------------------------


data Model = Model { _canvas       :: (SkiaCanvas.Canvas R)
                   , _points       :: IntMap.IntMap (Point 2 R)
                   , _diagram      :: Maybe [Point 2 R]
                   , __layers      :: Layers
                   , _canvasKitObj :: Maybe CanvasKit
                   , _surface   :: Maybe Surface
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
initialModel = (Model {})&canvas       .~ SkiaCanvas.blankCanvas 1024 768
                         &points       .~ mempty
                         &diagram      .~ Nothing
                         &layers       .~ initialLayers
                         &canvasKitObj .~ Nothing
                         &surface      .~ Nothing

--------------------------------------------------------------------------------

data Action = Id
            | OnLoad
            | InitializeSkCanvas CanvasKit Surface
            | SetCanvasSize !(Vector 2 Int)
            | CanvasAction SkiaCanvas.InternalCanvasAction
            | AddPoint
            | Draw
            -- | ToggleLayerStatus (Lens' Model Status)

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                            -> noEff m
    OnLoad                        -> m <# initializeCanvas theMainCanvasId
    InitializeSkCanvas ckObj surf -> noEff $ m&canvasKitObj ?~ ckObj
                                              &surface      ?~ surf
    SetCanvasSize v       -> noEff $ m&canvas.dimensions   .~ v
    CanvasAction ca       -> m&canvas %%~ flip SkiaCanvas.handleInternalCanvasAction ca
    AddPoint              -> addPoint
    Draw                  -> m <# case m^.canvasKitObj of
                                    Nothing -> pure Id -- TODO; error no canvasKit obj
                                    Just ck -> case m^.surface of
                                      Nothing   -> pure Id -- TODO: no surf
                                      Just surf -> do jsDraw ck surf
                                                      pure Id
    -- ToggleLayerStatus lr  -> noEff $ m&lr %~ toggleStatus
  where
    addPoint = recomputeDiagram m' <# pure Draw
      where
        m' = case m^.canvas.mouseCoordinates of
               Nothing -> m
               Just p  -> m&points %~ insertPoint p

runDraw               :: JSVal -> JS.JSCallAsFunction
runDraw canvasKit _ _ = \case
  [canvas] -> do jsg2 ("draw" :: MisoString) canvasKit canvas
                 pure ()
  _        -> pure ()


storeCanvasKitAndSurface                         :: MisoString -> JS.JSCallAsFunction
storeCanvasKitAndSurface theCanvasId _fObj _this = \case
  [canvasKit] -> do surface <- canvasKit ^.js1 ("MakeCanvasSurface" :: MisoString) theMainCanvasId
                               -- someshow store the canvasKit and the surface
                    runDraw' <- JS.function $ runDraw canvasKit
                    surface ^.js1 ("drawOnce" :: MisoString) runDraw'
                    pure ()
  _              -> pure ()

initializeCanvas             :: MisoString -> JSM Action
initializeCanvas theCanvasId = do
    theCanvasElem <- getElementById theCanvasId
    wVal <- theCanvasElem JS.! ("offsetWidth"  :: MisoString)
    hVal <- theCanvasElem JS.! ("offsetHeight" :: MisoString)
    (theCanvasElem JS.<# ("width"  :: MisoString)) wVal
    (theCanvasElem JS.<# ("height" :: MisoString)) hVal
    -- withCKFunction is the callback; i.e. the thing that we do with the given CanvasKit
    -- value
    withCKFunction <- JS.function $ storeCanvasKitAndSurface theCanvasId
    --  We grab the ckLoaded value,
    ckLoaded <- jsg ("ckLoaded" :: MisoString)
    jsg ("console" :: MisoString) ^. JS.js1 ("log" :: MisoString) ckLoaded
    -- liftIO $ print ckLoaded
    -- and call it with the '
    ckLoaded ^.js1 ("then" :: MisoString) withCKFunction
    w <- fromJSVal wVal
    h <- fromJSVal hVal
    case Vector2 <$> w <*> h of
      Nothing -> pure Id -- TODO: if we are here, something went wrong
      Just v  -> pure $ SetCanvasSize v



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
         , columns_ [ ]
                    [ leftPanel
                    , mainCanvas
                    , rightPanels
                    ]
         , footer
         ]
  where
    leftPanel  = div_ [class_ "column is-narrow has-background-light"]
                      [ text "woei"
                      ]
    mainCanvas = div_ [ class_ "column"
                      ]
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

    rightPanels = div_ [class_ "column is-2"]
                       [ overviewPanel
                       , layersPanel
                       ]
    overviewPanel = panel_ [text "Model"]
                           [text . ms . show $ m
                           , message_ Nothing        [] [text "foo"]
                           , message_ (Just Warning) [] [text "warning :)"]
                           ]

    layersPanel = panel_ [text "Layers"]
                         (map layer_ $ m^.layers.to allLayers)

    layer_   :: Layer -> View action
    layer_ l = label_ [class_ "panel-block"]
                      [ input_ [ type_ "checkbox"
                               , name_    $ l^.name
                               , checked_ $ l^.status == Visible
                               ]
                      , text $ l^.name
                      ]


    footer = footer_ [class_ "navbar is-fixed-bottom"]
                     [ navBarEnd_ [ p_ [class_ "navbar-item"]
                                       [ icon "fas fa-mouse-pointer"
                                       , text $ case over coordinates ms <$>
                                                     m^.canvas.mouseCoordinates of
                                           Nothing           -> "-"
                                           Just (Point2 x y) -> " (" <> x <> ", " <> y <> ")"
                                       ]
                                  ]
                     ]

    Vector2 w h = ms <$> m^.canvas.dimensions




columns_     :: [Attribute action] -> [View action] -> View action
columns_ ats = section_ ([class_ "columns"] <> ats)

menuBar_   :: Model -> View Action
menuBar_ _ = navBar_

  -- div_ [] [text "woei"]


  -- div_ []
  --                  [ text "woei"]


  -- div_ [ ]
  --                  [ either CanvasAction id <$>
  --                    Canvas.svgCanvas_ (m^.canvas)
  --                                      [ onClick AddPoint
  --                                      , styleInline_ "border: 1px solid black"
  --                                      ]
  --                                      canvasBody
  --                  , div_ [ onClick AddPoint ]
  --                         [text "add point" ]
  --                  , div_ []
  --                         [text . ms . show $ m^.canvas.mouseCoordinates ]
  --                  , div_ []
  --                         [text . ms . show $ m^.points ]
  --                   ]
  -- where
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

panel_             :: [View action] -> [View action] -> View action
panel_ heading chs =
    nav_ [class_ "panel"]
         ([ p_ [class_ "panel-heading"] heading]
          <> chs
         )

panelBlock_ = div_ [class_ "panel-block"]

panelTabs_ = p_ [class_ "panel-tabs"]

          -- <nav class="panel">
          --   <p class="panel-heading">Info</p>
          --   <div class="panel-block">
          --     <p class="control has-icons-left">
          --       <input class="input" type="text" placeholder="Search" />
          --       <span class="icon is-left">
          --         <i class="fas fa-search" aria-hidden="true"></i>
          --       </span>
          --     </p>
          --   </div>
          --   <p class="panel-tabs">
          --     <a class="is-active">All</a>
          --     <a>Public</a>
          --     <a>Private</a>
          --     <a>Sources</a>

          --   </p>

panelBlockActiveA_ = a_ [class_ "panel-block"]

          --   <a class="panel-block is-active">
          --     <span class="panel-icon">
          --       <i class="fas fa-book" aria-hidden="true"></i>
          --     </span>
          --     bulma
          --   </a>
          --   <a class="panel-block">
          --     <span class="panel-icon">
          --       <i class="fas fa-book" aria-hidden="true"></i>
          --     </span>
          --     marksheet
          --   </a>
          --   <a class="panel-block">
          --     <span class="panel-icon">
          --       <i class="fas fa-book" aria-hidden="true"></i>
          --     </span>
          --     minireset.css
          --   </a>
          --   <a class="panel-block">
          --     <span class="panel-icon">
          --       <i class="fas fa-book" aria-hidden="true"></i>
          --     </span>
          --     jgthms.github.io
          --   </a>
          --   <a class="panel-block">
          --     <span class="panel-icon">
          --       <i class="fas fa-code-branch" aria-hidden="true"></i>
          --     </span>
          --     daniellowtw/infboard
          --   </a>
          --   <a class="panel-block">
          --     <span class="panel-icon">
          --       <i class="fas fa-code-branch" aria-hidden="true"></i>
          --     </span>
          --     mojs
          --   </a>
          --   <label class="panel-block">
          --     <input type="checkbox" />
          --     remember me
          --   </label>
          --   <div class="panel-block">
          --     <button class="button is-link is-outlined is-fullwidth">
          --       Reset all filters
          --     </button>
          --   </div>
          -- </nav>


--------------------------------------------------------------------------------


data BulmaColor = Primary
                | Secondary
                | Dark
                | Link
                | Info
                | Success
                | Warning
                | Danger
                deriving (Show,Read,Eq)

colorClass :: BulmaColor -> MisoString
colorClass = \case
  Primary   -> "is-primary"
  Secondary -> "is-seconary"
  Dark      -> "is-dark"
  Link      -> "is-link"
  Info      -> "is-info"
  Success   -> "is-success"
  Warning   -> "is-warning"
  Danger    -> "is-danger"

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


--------------------------------------------------------------------------------

-- foreign import javascript unsafe "$r = initializeSkiaCanvas($1);"
--   jsInitializeSkiaCanvas :: MisoString -> JSM JSVal
jsInitializeSkiaCanvas :: MisoString -> JSM JSVal
jsInitializeSkiaCanvas = jsg1 ("initializeSkiaCanvas" :: MisoString)

jsDraw         :: CanvasKit -> Surface -> JSM ()
jsDraw ck surf = () <$ jsg2 ("draw" :: MisoString) (toJSVal ck) (toJSVal surf)

withPaint           :: CanvasKit -> (JSVal -> JSM a) -> JSM a
withPaint canvasKit =
    JSAddle.bracket (JS.new (canvasKit JS.! ("Paint" :: MisoString)) ())
                    (\paint -> paint ^. JS.js0 ("delete" :: MisoString))

withPath           :: CanvasKit -> (JSVal -> JSM a) -> JSM a
withPath canvasKit =
    JSAddle.bracket (JS.new (canvasKit JS.! ("Path" :: MisoString)) ())
                    (\path -> path ^. JS.js0 ("delete" :: MisoString))


-- data Cmd = Cmd OP

-- withPathFromCmds           :: CanvasKit -> [Cmd] -> (JSVal -> JSM a) -> JSM a
-- withPathFromCmds canvasKit =
--     JSAddle.bracket (JS.new (canvasKit JS.! ("Path.MakeFromCMDs" :: MisoString)) cmds)
--                     (\path -> path ^. JS.js0 ("delete" :: MisoString))
