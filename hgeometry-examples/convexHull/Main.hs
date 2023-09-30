{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

import           Control.Lens hiding (view, element)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           HGeometry.ConvexHull.GrahamScan
import           HGeometry.Ext
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Miso.Svg.Writer
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

type R = RealNumber 5

data Model = Model { _canvas   :: Canvas R
                   , _points   :: IntMap.IntMap [Point 2 R]
                   , _hull     :: Maybe (ConvexPolygon (Point 2 R :+ Int))
                   , _selected :: Maybe (Point 2 R :+ Int)
                   } deriving (Eq)
makeLenses ''Model

----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 1024  576) mempty Nothing Nothing

--------------------------------------------------------------------------------

data Action = Id
            | CanvasAction Canvas.InternalCanvasAction
            | AddPoint
            | Select (Point 2 R :+ Int)
            deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id               -> noEff m
    CanvasAction ca  -> do
                          c' <- Canvas.handleInternalCanvasAction (m^.canvas) ca
                          pure $ m&canvas .~ c'
    AddPoint         -> addPoint
    Select p         -> noEff $ m&selected .~ Just p
  where
    addPoint = noEff $ recomputeHull m'
       where
          m' = case m^.canvas.mouseCoordinates of
                 Nothing -> m
                 Just p  -> m&points %~ insertPoint p


recomputeHull   :: Model -> Model
recomputeHull m = let pts = NonEmpty.nonEmpty [ p :+ i | (i,p) <- IntMap.assocs (m^.points)]
                  in m&hull .~ fmap convexHull pts

insertPoint p m = let k = case IntMap.lookupMax m of
                            Nothing    -> 0
                            Just (i,_) -> succ i
                  in IntMap.insert k p m

--------------------------------------------------------------------------------

viewModel       :: Model -> View Action
viewModel m = div_ [ ]
                   [ either CanvasAction id <$>
                     Canvas.svgCanvas_ (m^.canvas)
                                       [ onClick AddPoint
                                       , id_ "mySvg"
                                       ]
                                       canvasBody
                   , div_ [ onClick AddPoint ]
                          [text . ms $ "add point" ]
                   , div_ []
                          [text . ms . show $ m^.canvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.points ]
                   , div_ []
                          [ div_ [] ["selected: "]
                          , text . ms . show $ m^.selected
                          ]
                    ]
  where
    canvasBody = [ draw pg [ stroke_ "red"
                           , fill_   "rgba(255, 0, 0, 0.6)"
                           ] | Just pg <- [m^.hull]]
              <> [ g_ [] [ draw p [ fill_ "black"
                                  , onClick $ Select p'
                                  ]
                         , textAt p [] (ms i)
                         ]
                 | p'@(p :+ i) <- m^.points ]
              <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $ mainJSM


mainJSM :: JSM ()
mainJSM = do
    let myApp = App { model         = initialModel
                    , update        = flip updateModel
                    , view          = viewModel
                    , subs          = [--  mouseSub (CanvasAction . GetMouseState)
                                      -- ,
                                        -- arrowsSub (CanvasAction . ArrowPress)
                                      ]
                    , events        = Map.insert "touchstart" False
                                    . Map.insert "touchmove" False
                                    . Map.insert "mousemove" False
                                    $ defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    , logLevel      = Off
                    }
    startApp myApp

textAt (Point2 x y) ats t = text_ ([ x_ $ ms x
                                  , y_ $ ms y
                                  ] <> ats
                                  ) [text t]
