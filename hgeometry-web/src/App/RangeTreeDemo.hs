{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App.RangeTreeDemo where


import           Control.Lens hiding (view, element)
import           Control.Monad.IO.Class
import           Data.Ext
import qualified Data.Geometry.Interactive.ICanvas as ICanvas
import           Data.Geometry.Interactive.ICanvas hiding (update, view)

import           Data.Geometry.Box
import           Data.Geometry.Interactive.Writer
import           Data.Geometry.Ipe (IpePage, content, readSinglePageFile, _IpePath)
import           Data.Geometry.Ipe.Attributes (Sing(..))
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ipe.FromIpe (_withAttrs, _asSomePolygon)
import           Data.Geometry.Point
import           Data.Geometry.RangeTree
import           Data.Geometry.RangeTree.Measure


import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.Bulma.Generic
import           Miso.Bulma.Panel
import           Miso.String (MisoString, ToMisoString, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

type QueryResult = String

data Mode = ViewMode | EditMode deriving (Show,Eq)


data Selection r = Partial (Point 2 r)
                 | Complete (Rectangle () r)
                 deriving (Eq,Show)
makePrisms ''Selection

data Model' r = Model { _iCanvas   :: ICanvas r
                      , _mode      :: Mode
                      , _points    :: [Point 2 r]
                      , _tree      :: Maybe (RangeTree 2 (Count :*: Report) () r)
                      , _selection :: Maybe (Selection r)
                      , _selected  :: Maybe QueryResult
                      } deriving (Show,Eq)
makeLenses ''Model'

type Model = Model' Rational
----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 980 800) ViewMode [] Nothing Nothing Nothing


--------------------------------------------------------------------------------

type Action = GAction Rational


-- data SelectionType = BeginSelect | CompleteSelect deriving (Show,Eq)
-- data SelectionAction r = SelectionAction SelectionType (Point 2 r) deriving (Show,Eq)

data GAction r = Id
               | GetKeyboard (Set Int)
               | CanvasAction CanvasAction
               | Click
               | SelectionAction
               | AddPoint
               | RunQuery
               deriving (Show,Eq)

editModeKey :: Int
editModeKey = 17

updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id               -> noEff m
    GetKeyboard s    -> let mode' = if editModeKey `Set.member` s then EditMode else ViewMode
                        in noEff $ m&mode .~ mode'
    CanvasAction ca  -> m&iCanvas %%~ flip ICanvas.update ca
    SelectionAction  -> case m^.iCanvas.mouseCoordinates of
                          Nothing -> noEff m
                          Just q  -> m&selection %%~ updateSelection q
    Click            -> case m^.mode of
      EditMode       -> m <# pure AddPoint
      ViewMode       -> m <# pure SelectionAction

    AddPoint         -> let q   = m^.iCanvas.mouseCoordinates
                            pts = maybeToList q <> m^.points
                        in noEff $ m&points .~ pts
                                    &tree   .~ (Just . buildTree $ pts)

    RunQuery         -> case m^?selection._Just._Complete of
                          Nothing -> noEff $ m&selected .~ Nothing
                          Just r  -> noEff $ m&selected .~ Just (runQuery r $ m^.tree)

buildTree :: [Point 2 Rational] -> RangeTree 2 (Count :*: Report) () Rational
buildTree = createRangeTree2 . NonEmpty.fromList . map ext


-- runQuery   :: (Ord r) => Rectangle p r -> [Point 2 r] -> Int
-- runQuery r = length . filter (`inBox` r)

runQuery   :: (Ord r, Show r, Show q) => Rectangle p r -> Maybe (RangeTree 2 (Count :*: Report) q r) -> String
runQuery r = let qr = extent r
             in maybe "" (show . search qr)


updateSelection    :: (Ord r)
                   => Point 2 r
                   -> Maybe (Selection r)
                   -> Effect Action (Maybe (Selection r))
updateSelection q = \case
      Nothing           -> noEff . Just . Partial  $ q
      Just (Partial p)  -> let sel = Just . Complete $ boundingBoxList' [p,q]
                           in sel <# pure RunQuery
      Just (Complete _) -> noEff . Just . Partial  $ q





--------------------------------------------------------------------------------

viewModel       :: Model -> View Action
viewModel m = div_ [ class_ "container"
                   ]
                   [ useBulmaRemote
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
                               , onClick Click
                               ]
                               (canvasBody m)
                ]

    rightBody = [ panel (infoAreaBody m) ]


canvasBody   :: Model -> [View Action]
canvasBody m = [ draw p [ stroke_ "black" ]
               | p <- m^.points
               ] <>
               drawSelectionWith (m^.iCanvas.mouseCoordinates) (m^.selection)

drawSelectionWith    :: Maybe (Point 2 Rational) -> Maybe (Selection Rational) -> [View action]
drawSelectionWith mq = \case
  Nothing -> []
  Just s -> case s of
    Partial p  -> maybe [] (\q -> [draw (boundingBoxList' [p,q]) [stroke_ "black"]]) mq
    Complete r -> [ draw r [ stroke_ "black"
                           , fill_ "red"
                           , opacity_ "0.7"
                           ]
                  ]

showMaybe :: Show a => Maybe a -> MisoString
showMaybe = maybe "Nothing" (ms . show)


infoAreaBody   :: Model -> [View Action]
infoAreaBody m = [ panelBlock [ panelIcon "fas fa-mouse-pointer"
                              , text . showMaybe $ m^.iCanvas.mouseCoordinates
                              ]
                 , panelBlock [ panelIcon "fas fa-microscope"
                              , viewSelected $ m^.selected
                              ]
                 , panelBlock [ div_ [] ["zoomlevel: "]
                              , text . ms . show $ m^.iCanvas.canvas.zoomLevel
                              ]
                 , panelBlock [ div_ [] ["Points: "]
                              , text . ms . show $ m^.points
                              ]
                 , panelBlock [ div_ [] ["Tree "]
                              , text . ms . show $ m^.tree
                              ]
                 , panelBlock [ div_ [] ["Selection: "]
                              , text . ms . show $ m^.selection
                              ]
                 , panelBlock [ div_ [] ["Query result: "]
                              , text . ms . show $ m^.selected
                              ]
                 ]

viewSelected :: Maybe QueryResult -> View action
viewSelected = \case
    Nothing -> text "Nothing"
    Just s -> text $ ms s


-- viewSelected    :: SubDiv Rational -> Maybe Selectable -> View Action
-- viewSelected ps = \case
--     Nothing -> text "Nothing"
--     Just s  -> case s of
--                  Vtx i -> view' i
--                  Edg i -> view' i
--                  Fce i -> view' i
--   where
--     view'   :: (HasDataOf (SubDiv Rational) i, Show (DataOf (SubDiv Rational) i), Show i)
--             => i -> View Action
--     view' i = div_ []
--                    [ field "id"
--                            [input_ [ class_ "input"
--                                    , type_ "text"
--                                    , readonly_ True
--                                    , disabled_ True

--                                    , value_ . ms . show $ i
--                                    ]
--                            ]
--                    , field "data"
--                            [input_ [ class_ "input"
--                                    , type_ "text"
--                                    , readonly_ True
--                                    , disabled_ True
--                                    , value_ . ms . show $ ps^.dataOf i
--                                    ]
--                            ]
--                    ]

--     field l bd = div_ [ class_ "field"]
--                       [ div_ [ class_ "field-label is-normal"]
--                              [ label_ [ class_ "label" ] [ text l ]]
--                       , div_ [ class_ "field-body"]
--                              [ div_ [ class_ "field" ]
--                                     [ p_ [ class_ "control" ]
--                                          bd
--                                     ]
--                              ]
--                       ]








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
                    , subs          = iCanvasSubs "mySvg" CanvasAction <>
                                      [ keyboardSub GetKeyboard
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
