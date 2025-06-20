{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

import           Control.Lens hiding (view, element)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           GHC.TypeNats
import           HGeometry.ConvexHull.GrahamScan
import           HGeometry.Ext
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso hiding (style_)
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Miso.Style (style_,(=:))
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

type R = RealNumber 5

data Model = Model { _canvas   :: Canvas R
                   , _points   :: IntMap.IntMap (Point 2 R)
                   , _hull     :: Maybe (ConvexPolygon (Point 2 R :+ Int))
                   , _selected :: Maybe (Point 2 R :+ Int)
                   } deriving (Eq)
makeLenses ''Model

--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 1024  576) mempty Nothing Nothing

--------------------------------------------------------------------------------

data Action = CanvasAction Canvas.InternalCanvasAction
            | AddPoint
            | Select (Point 2 R :+ Int)
            deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Model Action
updateModel m = \case
    CanvasAction ca  -> zoom canvas $ wrap Canvas.handleInternalCanvasAction ca
    AddPoint         -> addPoint
    Select p         -> noEff $ m&selected ?~ p
  where
    addPoint = noEff $ recomputeHull m'
       where
          m' = case m^.canvas.mouseCoordinates of
                 Nothing -> m
                 Just p  -> m&points %~ insertPoint p

recomputeHull   :: Model -> Model
recomputeHull m
  | m^.points.to length <= 2  = m&hull .~ Nothing
  | otherwise                 = let pts = NonEmpty.nonEmpty
                                          [ p :+ i | (i,p) <- IntMap.assocs (m^.points)]
                                in m&hull .~ fmap convexHull pts

insertPoint     :: p -> IntMap.IntMap p -> IntMap.IntMap p
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
                                       , style_ ["border" =: "1px solid black"]
                                       ]
                                       canvasBody
                   , div_ [ onClick AddPoint ]
                          [text $ "add point" ]
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
                                  , onClick $ Select (p :+ i)
                                  ]
                         , textAt p [] (ms i)
                         ]
                 | (i,p) <- m^..points.ifolded.withIndex ]
              -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $
         startComponent $
            Component
                { model         = initialModel
                , update        = wrap updateModel
                , view          = viewModel
                , subs          = mempty
                , events        = Canvas.withCanvasEvents defaultEvents
                , initialAction = Nothing
                , styles        = []
                , mountPoint    = Nothing
                , logLevel      = Off
                }

textAt                    :: ToMisoString r
                          => Point 2 r
                          -> [Attribute action] -> MisoString -> View action
textAt (Point2 x y) ats t = text_ ([ x_ $ ms x
                                  , y_ $ ms y
                                  ] <> ats
                                  ) [text t]


wrap       :: (model -> action -> Effect model action') -> action -> Effect model action'
wrap f act = get >>= flip f act
