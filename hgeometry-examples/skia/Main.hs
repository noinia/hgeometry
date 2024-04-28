{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

import           Control.Lens hiding (view, element)
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
import qualified Miso.Bulma.JSAddle as JSaddle
-- import           Miso.Bulma.Panel
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Options
import qualified SkiaCanvas
import           SkiaCanvas (mouseCoordinates, dimensions)

--------------------------------------------------------------------------------

type R = RealNumber 5

data Model = Model { _canvas   :: (SkiaCanvas.Canvas R)
                   , _points   :: IntMap.IntMap (Point 2 R)
                   , _diagram  :: Maybe [Point 2 R]
                   } deriving (Eq,Show)
makeLenses ''Model

--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

instance Default (Point 2 R :+ Int) where
  def = origin :+ 0

----------------------------------------

initialModel :: Model
initialModel = Model (SkiaCanvas.blankCanvas 1024 768) mempty Nothing

--------------------------------------------------------------------------------

data Action = Id
            | CanvasAction SkiaCanvas.InternalCanvasAction
            | AddPoint
            | Draw
            | OnLoad
            | SetCanvasDimensions !(Vector 2 Int)
            deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                    -> noEff m
    CanvasAction ca       -> m&canvas %%~ flip SkiaCanvas.handleInternalCanvasAction ca
    AddPoint              -> addPoint
    Draw                  -> noEff m
    SetCanvasDimensions v -> noEff $ m&canvas.dimensions .~ v
    OnLoad                -> m <# initializeCanvas theMainCanvasId
  where
    addPoint = noEff $ recomputeDiagram m'
      where
        m' = case m^.canvas.mouseCoordinates of
               Nothing -> m
               Just p  -> m&points %~ insertPoint p


initializeCanvas             :: MisoString -> JSM Action
initializeCanvas theCanvasId = do
  let w = 100 -- TODO
      h = 200
  pure $ SetCanvasDimensions (Vector2 w h)


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
         , columns_ []
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
                                    [ style_ $ Map.fromAscList
                                               [ ("width",      "100%")
                                               , ("height",     "100%")
                                               , ("min-width",  w <> "px")
                                               , ("min-height", h <> "px")
                                               ]
                                    , onClick AddPoint
                                    ]
                      ]

    rightPanels = div_ [class_ "column is-2"]
                       [ overviewPanel
                       , layersPanel
                       ]
    overviewPanel = panel_ [text "Model"]
                           [text . ms . show $ m]

    layersPanel = panel_ [text "Layers"]
                         [ label_ [class_ "panel-block"]
                                  [ input_ [type_ "checkbox"]
                                  , text "the layer name goes here"
                                  ]
                         ]


    footer = footer_ [class_ "navbar is-fixed-bottom"]
                     [ p_ [class_ "navbar-item"]
                          [text $ m^.canvas.mouseCoordinates.to show.to ms]
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
main = JSaddle.runWith Options.jsAddleOptions 8080 $
         startApp $
            App { model         = initialModel
                , update        = flip updateModel
                , view          = viewModel
                , subs          = mempty
                , events        = SkiaCanvas.withCanvasEvents defaultEvents
                , initialAction = Id
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
         [ navBarBrand_ ["brand"]
         , navBarBurger_  theMainMenuId
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
