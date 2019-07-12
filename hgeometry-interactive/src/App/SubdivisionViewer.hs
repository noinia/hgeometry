{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App.SubdivisionViewer where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Concurrent
import           Control.Lens hiding (view, element)
import           Data.Ext
import qualified Data.Geometry.Interactive.ICanvas as ICanvas
import           Data.Geometry.Interactive.ICanvas hiding (update, view)

import           Data.Geometry.Interactive.Writer
import           Data.Geometry.Ipe (IpePage, IpeObject, content, readSinglePageFile, _IpePath)
import           Data.Geometry.Ipe.FromIpe (_withAttrs, _asSimplePolygon)
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ipe.Value
import           Data.Geometry.Ipe.Attributes (Sing(..), attrLens)
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle.Warp as JSaddle
-- import qualified Language.Javascript.JSaddle.WebKitGTK as JSaddleWebkit
import           Miso
import           Miso.String (ms)
import           Miso.Subscription.MouseExtra
import           Miso.Svg hiding (height_, id_, style_, width_)
-- import           Touch

import qualified Graphics.UI.Webviewhs as WHS

--------------------------------------------------------------------------------

type Idx = Int

data Screen = Screen

data Selectable = Vtx (VertexId' Screen)
                | Edg (Dart Screen)
                | Fce (FaceId' Screen)
                deriving (Show,Eq)

data Model' r = Model { _iCanvas     :: ICanvas r
                      , _subdivision :: Maybe (PlanarSubdivision Screen
                                                                 ()
                                                                 (Maybe (IpeColor r))
                                                                 (Maybe (IpeColor r))
                                                                 r)
                      , _selected    :: Maybe Selectable
                      } deriving (Show,Eq)
makeLenses ''Model'

type Model = Model' Rational

----------------------------------------

loadInitialModel    :: FilePath -> IO Model
loadInitialModel fp = mkModel <$> readSinglePageFile fp
  where
    mkModel ep = mkModel' $ case ep^.._Right.content.traverse._withAttrs _IpePath _asSimplePolygon of
        []             -> Nothing
        ((p :+ ats):_) -> let ps = fromPolygon (Identity Screen) (toCounterClockWiseOrder p)
                                                                 (ats^.attrLens SFill)
                                                                 (Just red)
                          in Just $ ps&dartData.traverse._2 .~ (ats^.attrLens SStroke)
    mkModel' mp = Model (blankCanvas 980 800) mp Nothing

--------------------------------------------------------------------------------

type Action = GAction Rational

data GAction r = Id
               | CanvasAction CanvasAction
               | Select Selectable
               deriving (Show,Eq)

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                          -> noEff m
    CanvasAction ca             -> m&iCanvas %%~ flip ICanvas.update ca
    Select i                    -> noEff $ m&selected .~ Just i

--------------------------------------------------------------------------------

viewModel       :: Model -> View Action
viewModel m = div_ [ class_ "container"
                   , style_ $ Map.fromList [("margin-top", "20px")]
                   ]
                   [ bulmaLink
                   , div_ [ class_ "columns"]
                          [ div_ [ class_ "column is-9 has-background-link" ]
                                 [ ICanvas.view CanvasAction
                                               (m^.iCanvas)
                                               [ -- onClick AddPoint
                                                 id_ "mySvg"
                                               , class_ "has-background-white"
                                               ]
                                               (canvasBody m)
                                 ]
                          , div_ [ id_ "infoArea"
                                 , class_ "column is-3 has-background-primary"
                                 ]
                                 (infoAreaBody m)
                          ]
                   ]

canvasBody   :: Model -> [View Action]
canvasBody m = maybe [] drawPS $ m^.subdivision
  where
    drawPS ps = [ dPlanarSubdivisionWith dv de df ps []
                ]
    dv (i,vd) = Just $ draw (vd^.location) [ onClick $ Select (Vtx i)]
    de (i,e)  = Just $ draw (e^.core) [ onClick $ Select (Edg i)
                                      , stroke_ . msW "green" $ e^.extra
                                      ]
    df (i,f)  = Just $ draw (f^.core) [ onClick $ Select (Fce i)
                                      , fill_ . msW "blue" $ f^.extra
                                      ]
    msW t = maybe t ms


infoAreaBody   :: Model -> [View Action]
infoAreaBody m = [ div_ []
                        [text . ms . show $ m^.iCanvas.mouseCoordinates ]
                 -- , div_ []
                 --        [text . ms . show $ m^.points ]
                 , div_ []
                        [ div_ [] ["selected: "]
                        , text . ms . show $ m^.selected
                        ]
                 , div_ []
                        [ div_ [] ["panstatus: "]
                        , text . ms . show $ m^.iCanvas.panStatus
                        ]
                 , div_ []
                        [ div_ [] ["zoomlevel: "]
                        , text . ms . show $ m^.iCanvas.canvas.zoomLevel
                        ]
                 ]


bulmaLink :: View action
bulmaLink = link_ [ rel_ "stylesheet"
                  , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"
                  , textProp "integrity" "sha256-vK3UTo/8wHbaUn+dTQD0X6dzidqc5l7gczvH+Bnowwk="
                  , textProp "crossorigin" "anonymous"
                  ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
         initialModel <- loadInitialModel "hgeometry-interactive/resources/test.ipe"
         -- forkIO $ (
         JSaddle.run 8080 $ mainJSM initialModel
         --   )
         -- main2

mainJSM              :: Model -> JSM ()
mainJSM initialModel = do
    let myApp = App { model         = initialModel
                    , update        = flip updateModel
                    , view          = viewModel
                    , subs          = [ relativeMouseSub "mySvg" (CanvasAction . MouseMove)
                                      -- , relativeMouseSub "svgz" Mouse
                                        -- mouseSub (CanvasAction . MouseMove)
                                      , arrowsSub (CanvasAction . ArrowPress)
                                      ]
                    , events        = Map.insert "touchstart" False
                                    . Map.insert "touchmove"  False
                                    . Map.insert "mousemove"  False
                                    . Map.insert "wheel"      False
                                    $ defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    }
    startApp myApp


main2 :: IO ()
main2 = do
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "Test"
      , WHS.windowParamsUri        = "http://localhost:8080"
      , WHS.windowParamsWidth      = 1600
      , WHS.windowParamsHeight     = 1000
      , WHS.windowParamsResizable  = False
      , WHS.windowParamsDebuggable = True
      }
