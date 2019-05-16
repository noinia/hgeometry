{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Svg where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Lens hiding (view, element)
import           Data.Ext
import           Data.Geometry.Interactive.Canvas hiding (update, view)
import qualified Data.Geometry.Interactive.Canvas as ICanvas

import           Data.Geometry.Interactive.Writer
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.Geometry.Vector
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.Subscription.MouseExtra
import qualified Miso.Svg.Event as Event
import           Miso.String (MisoString, ToMisoString, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
-- import           Touch

--------------------------------------------------------------------------------

type Idx = Int

data CHModel r = CHModel { _iCanvas  :: ICanvas r
                         , _points   :: [Point 2 r :+ Int]
                         , _hull     :: Maybe (ConvexPolygon Int r)
                         , _nextNum  :: Int
                         , _selected :: Maybe (Point 2 r :+ Int)
                         } deriving (Show,Eq)
makeLenses ''CHModel

type Model = CHModel Rational

----------------------------------------

initialModel :: Model
initialModel = CHModel blankCanvas [] Nothing 1 Nothing

blankCanvas :: Num r => ICanvas r
blankCanvas = ICanvas (createCanvas 1024 576) Nothing

--------------------------------------------------------------------------------

type Action = GAction Rational

data GAction r = Id
               | CanvasAction CanvasAction
               | AddPoint
               | Select (Point 2 r :+ Int)
               deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                          -> noEff m
    CanvasAction ca             -> do
                                     c' <- ICanvas.update (m^.iCanvas) ca
                                     pure $ m&iCanvas .~ c'
    AddPoint                    -> addPoint
    Select p                    -> noEff $ m&selected .~ Just p
  where
    addPoint = noEff $ recomputeHull m'
       where
          m' = m&points  %~ (np <>)
                &nextNum %~ (+1)
          np = maybe [] (\p -> [p :+ m^.nextNum]) $ m^.iCanvas.mouseCoordinates


recomputeHull m = m&hull .~ fmap convexHull (NonEmpty.nonEmpty $ m^.points)

--------------------------------------------------------------------------------

viewModel       :: Model -> View Action
viewModel model = div_ [ ]
                       [ ICanvas.view (model^.iCanvas)
                                      [ onClick AddPoint
                                      , id_ "mySvg"
                                      ]
                                      canvasBody
                       , div_ [ onClick AddPoint ]
                              [text . ms $ model^.nextNum ]
                       , div_ []
                              [text . ms . show $ model^.iCanvas.mouseCoordinates ]
                       , div_ []
                              [text . ms . show $ model^.points ]
                       , div_ []
                              [ div_ [] ["selected: "]
                              , text . ms . show $ model^.selected
                              ]
                       ]
  where
    canvasBody = [ draw pg [ stroke_ "red"
                           , fill_   "rgba(255, 0, 0, 0.6)"
                           ] | Just pg <- [model^.hull]]
              <> [ g_ [] [ draw p [ fill_ "black"
                                  , onClick $ Select p'
                                  ]
                         , textAt p [] (ms i)
                         ]
                 | p'@(p :+ i) <- model^.points ]
              <> [ draw p [ fill_ "blue" ]  | Just p <- [model^.iCanvas.mouseCoordinates] ]

--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $ mainJSM


mainJSM :: JSM ()
mainJSM = do
    let myApp = App { model         = initialModel
                    , update        = flip updateModel
                    , view          = viewModel
                    , subs          = [ --relativeMouseSub "mySvg" (CanvasAction . GetMouseState)
                                        mouseSub (CanvasAction . GetMouseState)
                                      , arrowsSub (CanvasAction . GetArrowsState)
                                      ]
                    , events        = Map.insert "touchstart" False
                                    . Map.insert "touchmove" False
                                    . Map.insert "mousemove" False
                                    $ defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    }
    startApp myApp
