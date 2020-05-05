{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App.IpeViewer where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Lens hiding (view, element)
import           Data.Ext
import           Data.Geometry.Web.ICanvas hiding (update, view)
import qualified Data.Geometry.Web.ICanvas as ICanvas

import           Data.Geometry.Web.Writer
import           Data.Geometry.Ipe(IpePage, IpeObject, content, readSinglePageFile)
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (ms)
import           Miso.Subscription.MouseExtra
import           Miso.Svg hiding (height_, id_, style_, width_)
-- import           Touch

--------------------------------------------------------------------------------

type Idx = Int

data Screen = Screen

-- data PSModel r = PSModel { _iCanvas  :: ICanvas r
--                          , _subdiv   :: PlanarSubdivision Screen () () () r
--                          }


data Model' r = Model { _iCanvas  :: ICanvas r
                      , _ipePage  :: Maybe (IpePage r)
                      , _selected :: Maybe (IpeObject r)
                      } deriving (Show,Eq)
makeLenses ''Model'

type Model = Model' Rational

----------------------------------------

loadInitialModel    :: FilePath -> IO Model
loadInitialModel fp = mkModel <$> readSinglePageFile fp
  where
    mkModel ep = mkModel' $ case ep of
        Left _  -> Nothing
        Right p -> Just p
    mkModel' mp = Model (blankCanvas 980 800) mp Nothing

--------------------------------------------------------------------------------

type Action = GAction Rational

data GAction r = Id
               | CanvasAction CanvasAction
               | Select (IpeObject r)
               deriving (Show,Eq)

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                          -> noEff m
    CanvasAction ca             -> m&iCanvas %%~ flip ICanvas.update ca
    Select p                    -> noEff $ m&selected .~ Just p

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
canvasBody m = [ rect_ [ fill_ "red"
                         , x_ "0"
                         , y_ "0"
                         , width_ "100"
                         , height_ "100"
                         ] [] ]
               <> [ draw o [onClick $ Select o ]
                  | o <- m^.ipePage._Just.content
                  ]
               -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.iCanvas.mouseCoordinates] ]





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
         JSaddle.run 8080 $ mainJSM initialModel

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
