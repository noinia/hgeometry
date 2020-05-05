{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App.Viewer where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Lens hiding (view, element)
import           Data.Ext
import           Data.Geometry.Web.ICanvas hiding (update, view)
import qualified Data.Geometry.Web.ICanvas as ICanvas

import           Data.Geometry.Web.Writer
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


data CHModel r = CHModel { _iCanvas  :: ICanvas r
                         , _points   :: [Point 2 r :+ Int]
                         , _hull     :: Maybe (ConvexPolygon Int r)
                         , _nextNum  :: Int
                         , _selected :: Maybe (Point 2 r :+ Int)
                         , _mousePositionX :: Maybe (Point 2 Int)
                         } deriving (Show,Eq)
makeLenses ''CHModel

type Model = CHModel Rational

----------------------------------------

initialModel :: Model
initialModel = CHModel (blankCanvas 980 800) pts Nothing 1 Nothing Nothing
  where
    pts = [ Point2 100 200 :+ 1
          , Point2 323 253 :+ 2
          , Point2 346 532 :+ 3
          ]

--------------------------------------------------------------------------------

type Action = GAction Rational

data GAction r = Id
               | CanvasAction CanvasAction
               | Mouse (Int,Int)
               | Select (Point 2 r :+ Int)
               deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                          -> noEff m
    CanvasAction ca             -> do
                                     c' <- ICanvas.update (m^.iCanvas) ca
                                     pure $ m&iCanvas .~ c'
    Select p                    -> noEff $ m&selected .~ Just p
    Mouse (x,y)         -> let p  = Point2 x y
                           in noEff $ m&mousePositionX .~ Just p


recomputeHull :: (Ord r, Num r) => CHModel r -> CHModel r
recomputeHull m = m&hull .~ fmap convexHull (NonEmpty.nonEmpty $ m^.points)

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
canvasBody m = [ draw pg [ stroke_ "red"
                         , fill_   "rgba(255, 0, 0, 0.6)"
                         ] | Just pg <- [m^.hull]]
               <> [rect_ [ fill_ "red"
                         , x_ "0"
                         , y_ "0"
                         , width_ "100"
                         , height_ "100"
                         ] []]
               <> [ g_ [] [ draw p [ fill_ "black"
                                   , onClick $ Select p'
                                   ]
                          , textAt p [] (ms i)
                          ]
                  | p'@(p :+ i) <- m^.points ]
               -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.iCanvas.mouseCoordinates] ]

infoAreaBody   :: Model -> [View Action]
infoAreaBody m = [ div_ []
                        [text . ms . show $ m^.iCanvas.mouseCoordinates ]
                 , div_ []
                        [text . ms . show $ m^.points ]
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
main = JSaddle.run 8080 $ mainJSM


mainJSM :: JSM ()
mainJSM = do
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
