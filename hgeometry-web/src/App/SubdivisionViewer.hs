{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App.SubdivisionViewer where


import           Control.Lens hiding (view, element)
import           Control.Monad.IO.Class
import           Data.Ext
import qualified Data.Geometry.Web.ICanvas as ICanvas
import           Data.Geometry.Web.ICanvas hiding (update, view)

import           Data.Geometry.Web.Writer
import           Data.Geometry.Ipe (IpePage, content, readSinglePageFile, _IpePath)
import           Data.Geometry.Ipe.Attributes (Sing(..),_Attr)
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ipe.FromIpe (_withAttrs, _asSomePolygon)
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.Bulma.Generic
import           Miso.Bulma.Panel
import           Miso.String (MisoString, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

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

initialModel :: Model
initialModel = Model (blankCanvas 980 800) Nothing Nothing

-- loadInitialModel    :: FilePath -> IO Model
-- loadInitialModel fp = mkModel <$> loadFromFile fp
--   where
--     mkModel mp =

loadFromFile    :: FilePath -> IO (Maybe (SubDiv Rational))
loadFromFile fp = buildPS <$> readSinglePageFile fp

----------------------------------------
-- * From Multiple simple polygons

buildPS    :: Either e (IpePage Rational) -> Maybe (SubDiv Rational)
buildPS ep = build <$> NonEmpty.nonEmpty (fmap f polies)
  where
    polies = ep^.._Right.content.traverse._withAttrs _IpePath _asSomePolygon
    f (p :+ ats) = bimap toCounterClockWiseOrder toCounterClockWiseOrder p :+ ats^?_Attr SFill

    build pgs = let ps = fromPolygons' (Identity Screen) (Just $ named "red") pgs
                in ps&dartData.traverse._2 .~ Just (named "black")

--------------------------------------------------------------------------------

type Action = GAction Rational

data GAction r = Id
               | CanvasAction CanvasAction
               | Select Selectable
               | LoadSubdiv (Maybe (SubDiv Rational))
               | OpenFile FilePath
               deriving (Show,Eq)

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id               -> noEff m
    CanvasAction ca  -> m&iCanvas %%~ flip ICanvas.update ca
    Select i         -> noEff $ m&selected .~ Just i
    LoadSubdiv mps   -> noEff $ m&subdivision .~ mps
    OpenFile fp      -> m <# do mps <- liftIO $ loadFromFile fp
                                pure $ LoadSubdiv mps

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
                   , div_ [ class_ "has-background-grey-lighter columns"]
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
                       , class_ "panel"
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



showMaybe :: Show a => Maybe a -> MisoString
showMaybe = maybe "Nothing" (ms . show)



infoAreaBody   :: Model -> [View Action]
infoAreaBody m = [ panelBlock [ panelIcon "fas fa-mouse-pointer"
                              , text . showMaybe $ m^.iCanvas.mouseCoordinates
                              ]
                 , panelBlock [ panelIcon "fas fa-microscope"
                              , viewSelected'
                              ]
                 , panelBlock [ div_ [] ["zoomlevel: "]
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








--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- main :: IO ()
main :: IO ()
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
                    , subs          = iCanvasSubs "mySvg" CanvasAction
                    , events        = Map.insert "touchstart" False
                                    . Map.insert "touchmove"  False
                                    . Map.insert "mousemove"  False
                                    . Map.insert "wheel"      False
                                    $ defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    }
    startApp myApp
