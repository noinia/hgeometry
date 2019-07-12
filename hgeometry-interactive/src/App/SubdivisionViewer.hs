{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PartialTypeSignatures      #-}
module App.SubdivisionViewer where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Concurrent
import           Control.Lens hiding (view, element)
import           Data.Ext
import qualified Data.Geometry.Interactive.ICanvas as ICanvas
import           Data.Geometry.Interactive.ICanvas hiding (update, view)

import           Data.Geometry.Interactive.Writer
import           Data.Geometry.Ipe (IpePage, IpeObject, content, readSinglePageFile, _IpePath)
import           Data.Geometry.Ipe.FromIpe (_withAttrs, _asSomePolygon, _asSimplePolygon)
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
import           Miso.String (MisoString, ToMisoString, ms)
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

type SubDiv r = PlanarSubdivision Screen ()
                                         (Maybe (IpeColor r))
                                         (Maybe (IpeColor r))
                                         r

data Model' r = Model { _iCanvas     :: ICanvas r
                      , _subdivision :: Maybe (SubDiv r)
                      , _selected    :: Maybe Selectable
                      } deriving (Show,Eq)
makeLenses ''Model'

type Model = Model' Rational

----------------------------------------


loadInitialModel    :: FilePath -> IO Model
loadInitialModel fp = mkModel . buildPS <$> readSinglePageFile fp
  where
    mkModel mp = Model (blankCanvas 980 800) mp Nothing

    -- buildPS ep = case ep^.._Right.content.traverse._withAttrs _IpePath _asSomePolygon of
    --     []             -> Nothing
    --     ((p :+ ats):_) -> let ps = either build build p (ats^.attrLens SFill) (Just red)
    --                       in Just $ ps&dartData.traverse._2 .~ (ats^.attrLens SStroke)

buildPS    :: Either e (IpePage Rational) -> Maybe (SubDiv Rational)
buildPS ep = build <$> NonEmpty.nonEmpty (fmap f polies)
  where
    polies = ep^.._Right.content.traverse._withAttrs _IpePath _asSimplePolygon
    f (p :+ ats) = toCounterClockWiseOrder p :+ (ats^.attrLens SFill)

    build pgs = let ps = fromSimplePolygons (Identity Screen) (Just $ named "red") pgs
                in ps&dartData.traverse._2 .~ Just (named "black")


    -- build   :: (Ord r, Fractional r)
    --         => Polygon t () r -> f -> f
    --         -> PlanarSubdivision Screen () () f r
    -- build p = fromPolygon (Identity Screen) (toCounterClockWiseOrder p)





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
viewModel m = div_ [ class_ "container has-background-grey-lighter"
                   , style_ $ Map.fromList [("margin-top", "20px")]
                   ]
                   [ useBulmaRemote
                   , div_ [ class_ "columns"]
                          [ div_ [ class_ "column" ]
                                 leftBody
                          , div_ [ class_ "column is-one-third" ]
                                 rightBody
                          ]
                   ]
  where
    leftBody  = [ ICanvas.view CanvasAction (m^.iCanvas)
                               [ id_ "mySvg"
                               , class_ "has-background-white"
                               ]
                               (canvasBody m)
                ]

    rightBody = [ div_ [ id_ "infoArea"
                       , class_ "panel container"
                       ]
                       (infoAreaBody m)
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

icon    :: MisoString -> View action
icon cs = i_ [ class_ cs, textProp "aria-hidden" "true"] []


showMaybe :: Show a => Maybe a -> MisoString
showMaybe = maybe "Nothing" (ms . show)

infoAreaBody   :: Model -> [View Action]
infoAreaBody m = [ div_ [ class_ "panel-block"]
                        [ span_ [ class_ "panel-icon"]
                                [ icon "fas fa-mouse-pointer"]
                        , text . showMaybe $ m^.iCanvas.mouseCoordinates
                        ]
                 , div_ [ class_ "panel-block" ]
                        [ span_ [ class_ "panel-icon"]
                                [ icon "fas fa-microscope"]
                        , viewSelected'
                        ]
                 , div_ [ class_ "panel-block" ]
                        [ div_ [] ["zoomlevel: "]
                        , text . ms . show $ m^.iCanvas.canvas.zoomLevel
                        ]
                 ]
  where
    viewSelected' = case m^.subdivision of
                      Nothing -> "Nothing"
                      Just ps -> viewSelected ps $ m^.selected

viewSelected    :: SubDiv Rational -> Maybe Selectable -> View Action
viewSelected ps = \case
    Nothing -> text "Nothing"
    Just s  -> case s of
                 Vtx i -> view' i
                 Edg i -> view' i
                 Fce i -> view' i
  where
    view'   :: (HasDataOf (SubDiv Rational) i, Show (DataOf (SubDiv Rational) i), Show i)
            => i -> View Action
    view' i = div_ []
                   [ field "id"
                           [input_ [ class_ "input"
                                   , type_ "text"
                                   , readonly_ True
                                   , disabled_ True
                                   , value_ . ms . show $ i
                                   ]
                           ]
                   , field "data"
                           [input_ [ class_ "input"
                                   , type_ "text"
                                   , readonly_ True
                                   , disabled_ True
                                   , value_ . ms . show $ ps^.dataOf i
                                   ]
                           ]
                   ]

    field l bd = div_ [ class_ "field"]
                      [ div_ [ class_ "field-label is-normal"]
                             [ label_ [ class_ "label" ] [ text l ]]
                      , div_ [ class_ "field-body"]
                             [ div_ [ class_ "field" ]
                                    [ p_ [ class_ "control" ]
                                         bd
                                    ]
                             ]
                      ]



useBulmaRemote :: View action
useBulmaRemote = div_ [] [bulmaLink,iconLink]

bulmaLink :: View action
bulmaLink = link_ [ rel_ "stylesheet"
                   , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"
                   , textProp "integrity" "sha256-vK3UTo/8wHbaUn+dTQD0X6dzidqc5l7gczvH+Bnowwk="
                   , textProp "crossorigin" "anonymous"
                   ]

iconLink :: View action
iconLink = Miso.script_ [ src_ "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
                        , defer_ "true"
                        ] []


--------------------------------------------------------------------------------

-- main :: IO ()
main = do
         initialModel <- loadInitialModel "hgeometry-interactive/resources/test.ipe"
         -- let Just ps = initialModel^.subdivision
         -- return ps

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
