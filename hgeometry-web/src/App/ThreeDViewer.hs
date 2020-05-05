{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PartialTypeSignatures      #-}
module App.SubdivisionViewer where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Control.Concurrent
import           Control.Lens hiding (view, element)
import           Control.Monad.IO.Class
import           Data.Ext
import qualified Data.Geometry.Web.ICanvas as ICanvas
import           Data.Geometry.Web.ICanvas hiding (update, view)

import           Data.Geometry.Web.Writer
import           Data.Geometry.Ipe (IpePage, IpeObject, content, readSinglePageFile, _IpePath)
import           Data.Geometry.Ipe.FromIpe (_withAttrs, _asSomePolygon, _asSimplePolygon, _asMultiPolygon)
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ipe.Value
import           Data.Geometry.Ipe.Attributes (Sing(..), attr)
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Vector
import           Data.Geometry.Transformation
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle.Warp as JSaddle
-- import qualified Language.Javascript.JSaddle.WebKitGTK as JSaddleWebkit
import           Miso
import           Miso.Component.Menu
import           Miso.String (MisoString, ToMisoString(..), ms)
import           Miso.Subscription.MouseExtra
import           Miso.Event.Decoder (valueDecoder)
import           Miso.Html.Event (on)
import           Miso.Svg hiding (height_, id_, style_, width_)
-- import           Touch



import qualified Graphics.UI.Webviewhs as WHS

import           Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval (EdgeSide(..), Tri)
import qualified Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval as HiddenSurfaceRemoval
import           Data.Geometry.Arrangement
import           Data.Geometry.Box (Rectangle,box)
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import           Graphics.Camera
import           Graphics.Render

--------------------------------------------------------------------------------

type Idx = Int

data Screen = Screen

data Selectable = Vtx (VertexId' Screen)
                | Edg (Dart Screen)
                | Fce (FaceId' Screen)
                deriving (Show,Eq)


type Scene r = [Triangle 3 () r :+ IpeColor r]



type Arr r = Arrangement Screen
                              (NonEmpty.NonEmpty (Tri () () (IpeColor r) r))
                              ()
                              (Maybe EdgeSide)
                              (Maybe (IpeColor r))
                              r

type SubDiv r = PlanarSubdivision Screen ()
                                         (Maybe EdgeSide)
                                         (Maybe (IpeColor r))
                                         r

data Model' r = Model { _iCanvas       :: ICanvas r
                      , _scene         :: Maybe (Scene r)
                      , _camera        :: Camera r
                      , _myArr         :: Maybe (Arr r)
                      , _selected      :: Maybe Selectable
                      } deriving (Show,Eq)
makeLenses ''Model'

type Model = Model' Rational

----------------------------------------

mkBottom :: r -> (r,r) -> (r,r) -> c -> [Triangle 3 () r :+ c]
mkBottom z (lx, ly) (rx, ry) c = [ triangle' (Point3 lx ly z)
                                             (Point3 rx ly z)
                                             (Point3 lx ry z) :+ c
                                 , triangle' (Point3 lx ry z)
                                             (Point3 rx ry z)
                                             (Point3 rx ly z) :+ c
                                 ]




-- xSide :: [Triangle 3 p r :+ c] -> [Triangle 3 p r :+ c]
-- xSide = map (\(t :+ c) -> pmap toX t :+ c)
--   where
--     toX (Point3 x y z) = Point3 z y x

-- ySide :: [Triangle 3 p r :+ c] -> [Triangle 3 p r :+ c]
-- ySide = map (\(t :+ c) -> pmap toY t :+ c)
--   where
--     toY (Point3 x y z) = Point3 x z y


myT :: Triangle 3 () Rational
myT = Triangle (ext $ Point3 1  1  10)
               (ext $ Point3 20 1  10)
               (ext $ Point3 20 30 10)


-- FIXME 20 causes zero error
topSide z = [ triangle' (Point3 0   0 z)
                        (Point3 17  0 z)
                        (Point3 17 17 z) :+ purple
            , triangle' (Point3 0   0 z)
                        (Point3 0  17 z)
                        (Point3 17 17 z) :+ purple
            ]

leftSide x = [ triangle' (Point3 x 0   0)
                         (Point3 x 17  0)
                         (Point3 x 17 17) :+ navy
             , triangle' (Point3 x 0   0)
                         (Point3 x 0  17)
                         (Point3 x 17 17) :+ navy
             ]

cube = topSide 17 <> topSide 0 <> leftSide 0 <> leftSide 23

myScene :: Scene Rational
myScene = [ myT :+ seagreen
          -- , triangle' origin
          --             (Point3 0 40 (-10))
          --             (Point3 0 0  (-10)) :+ purple
          -- , triangle' (Point3 0 0 (-50))
          --             (Point3 0 40 (-10))
          --             (Point3 0 0  (-10)) :+ navy
          ] <> cube
        ++ axes
        -- ++ cube

axes = [ triangle' origin
                   (Point3 500 0 (-10))
                   (Point3 500 0 0) :+ red
       , triangle' origin
                   (Point3 0 500 (-10))
                   (Point3 0 500 0) :+ green
       , triangle' origin
                   (Point3 0 (-10) 49)
                   (Point3 0 0     49) :+ blue
       ]


----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 980 800 & capabilities .~ mempty
                     )
                     (Just myScene)
                     myCamera
                     (Just $ renderAll myCamera myScene)
                     Nothing

myCamera :: Camera Rational
myCamera = Camera (Point3 (-30) (-20) 20)
                  (Vector3 0 0 (-1))
                  (Vector3 0 1 0)
                  10
                  15
                  55
                  (Vector2 980 800)


-- loadInitialModel    :: FilePath -> IO Model
-- loadInitialModel fp = mkModel <$> loadFromFile fp
--   where
--     mkModel mp =

-- loadFromFile    :: FilePath -> IO (Maybe (SubDiv Rational))
-- loadFromFile fp = buildPS <$> readSinglePageFile fp

--------------------------------------------------------------------------------

type Action = GAction Rational

data GAction r = Id
               | CanvasAction CanvasAction
               | Select Selectable
               | GetArrows Arrows
               -- | LoadSubdiv (Maybe (SubDiv Rational))
               -- | OpenFile FilePath
               deriving (Show,Eq)

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id               -> noEff m
    CanvasAction ca  -> m&iCanvas %%~ flip ICanvas.update ca
    Select i         -> (m&selected .~ Just i) <# do liftIO $ print i
                                                     pure Id
    GetArrows arr    -> noEff . reRender $ m&camera %~ shiftCamera arr


reRender   :: Model -> Model
reRender m = m&myArr .~ (renderAll (m^.camera) <$> m^.scene)

    -- LoadSubdiv mps   -> noEff $ m&subdivision .~ mps
    -- OpenFile fp      -> m <# do mps <- liftIO $ loadFromFile fp
    --                             pure $ LoadSubdiv mps

shiftCamera                :: Num r => Arrows -> Camera r -> Camera r
shiftCamera (Arrows x y) c = c&cameraPosition %~ (.+^ 5 *^ v)
  where
    v = fromIntegral <$> Vector3 x y 0

--------------------------------------------------------------------------------

-- mData :: Menu Action
-- mData = Menu [ MenuItem "File" Nothing
--                [ MenuChild "Open"
--                      $ Just (OpenFile "hgeometry-interactive/resources/testmulti.ipe")]
--              , MenuItem "Edit" Nothing
--                [MenuChild "Child 1" Nothing]
--              ]


-- onUpload :: (FilePath -> action) -> Attribute action
-- onUpload = on "change" valueDecoder'
--   where
--     Decoder f t = valueDecoder
--     valueDecoder' = Decoder (fmap fromMisoString . f) t

viewModel       :: Model -> View Action
viewModel m = div_ [ class_ "container"
                   ]
                   [ useBulmaRemote
                   -- , menu mData
                   -- , input_ [ type_ "file", id_ "open-file"
                   --          , onUpload OpenFile
                   --          ]
                   , div_ [ class_ "columns has-background-grey-lighter"]
                          [ div_ [ class_ "column"
                                 , style_ $ Map.fromList [("padding-top", "20px")]
                                 ]
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
canvasBody m = maybe [] (\arr -> [drawColoredArrangement arr []]) $ m^.myArr

-- drawScene     :: (RealFrac r, Ord r, ToMisoString r
--                  , Show r, Read r)
--               => Camera r -> Scene r -> View Action
-- drawScene c s = drawColoredArrangement arr []
--   where
--     arr = renderAll c s

drawColoredArrangement     :: ToMisoString r => Arrangement Screen _ _ _ (Maybe (IpeColor r)) r
                           -> [Attribute Action] -> View Action
drawColoredArrangement arr = dPlanarSubdivisionWith dv de df subdiv
  where
    black :: IpeColor Rational
    black = named "black"
    subdiv = (arr^.subdivision)&vertexData.traverse     .~ black
                               &dartData.traverse._2    .~ black

    dv (i,vd) = Nothing -- Just $ draw (vd^.location) [ onClick $ Select (Vtx i)]
    -- de _      = Nothing
    de (i,e)  = Just $ draw (e^.core) [ -- onClick $ Select (Edg i)
                                      -- ,
                                        stroke_  . ms $ e^.extra
                                      , opacity_ "0.5"
                                      ]
    df (i,f)  = Just $ draw (f^.core) [ -- onClick $ Select (Fce i)
                                      -- ,
                                        fill_ . msW "none" $ f^.extra
                                      ]
    msW t = maybe t ms


    -- f :: IpeColor r -> IpeAttributes Path r
    -- f = attr S

      -- Expected type: Arrangement
      --                  Screen
      --                  (NonEmpty.NonEmpty (Tri () () (IpeColor r) r))
      --                  (Maybe (View action))
      --                  (Maybe (View action))
      --                  (Maybe (View action))
      --                  r
      --   Actual type: Arrangement
      --                  Screen
      --                  (NonEmpty.NonEmpty (Tri () () (IpeColor r) r))
      --                  ()
      --                  (Maybe EdgeSide)
      --                  (Maybe (IpeColor r))
      --                  r



----------------------------------------

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
                        [ div_ [] ["camera: "]
                        , text . ms . show $ m^.camera
                        ]
                 ]
  where
    viewSelected' = case m^?myArr._Just.subdivision of
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

--------------------------------------------------------------------------------

renderAll      :: forall r. (Ord r, Fractional r, RealFrac r
                  , Show r, ToMisoString r, Read r
                  )
               => Camera r -> Scene r
               -> Arrangement Screen
                              (NonEmpty.NonEmpty (Tri () () (IpeColor r) r))
                              ()
                              (Maybe EdgeSide)
                              (Maybe (IpeColor r))
                              r
renderAll c s = arr&subdivision.faceData %~ fmap mkColor
  where
    arr = HiddenSurfaceRemoval.render (Identity Screen) (c^.cameraPosition) rect ts
    ts = renderScene' c s
    -- ts = HiddenSurfaceRemoval.scene
    mkColor = fmap (^.extra.extra)
    rect :: Rectangle () r
    rect = box (ext origin) (ext . Point $ c^.screenDimensions)



renderScene     :: Fractional r => Camera r -> Scene r
                -> [Triangle 2 () r :+ IpeColor r]
renderScene c s = over core (renderTriangle t) <$> s
  where
    !t = cameraTransform c

renderScene'     :: Fractional r => Camera r -> Scene r
                 -> [Triangle 2 () r :+ (Triangle 3 () r :+ IpeColor r)]
renderScene' c s = (\t3 -> (renderTriangle t $ t3^.core) :+ t3) <$> s
  where
    !t = cameraTransform c

--------------------------------------------------------------------------------


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
         -- initialModel <- loadInitialModel "hgeometry-interactive/resources/testmulti.ipe"
         -- let Just ps = initialModel^.subdivision
         -- return ps

         -- forkIO $ (
         JSaddle.run 8080 mainJSM
         --   )
         -- main2

mainJSM :: JSM ()
mainJSM = do
    let myApp = App { model         = initialModel
                    , update        = flip updateModel
                    , view          = viewModel
                    , subs          = [ relativeMouseSub "mySvg" (CanvasAction . MouseMove)
                                      -- , relativeMouseSub "svgz" Mouse
                                        -- mouseSub (CanvasAction . MouseMove)
                                      , arrowsSub GetArrows
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
