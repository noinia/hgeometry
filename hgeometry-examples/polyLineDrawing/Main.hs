{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import           Control.Monad.IO.Class

import           Control.Lens hiding (view, element)
import           Data.Colour.SRGB
import           Data.Default.Class
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import qualified Data.Sequence as Sequence
import           Data.Word
import           GHC.TypeNats
import           HGeometry.Ext
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates, dimensions)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
-- import           HGeometry.PolyLine.Simplification.DouglasPeucker
import           HGeometry.Sequence.NonEmpty
import           HGeometry.Vector
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso hiding (onMouseUp, onMouseDown)
import           Miso.Event.Extra
import qualified Miso.Html.Element as Html
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5

type PolyLine' r = PolyLineF ViewR1 (Point 2 r)

data PartialPolyLine r = StartPoint (Point 2 r)
                       | PartialPolyLine (PolyLine' r)
                       deriving (Show,Eq)
makePrisms ''PartialPolyLine

instance ToMisoString r => Drawable (PartialPolyLine r) where
  draw = \case
    StartPoint s      -> draw s
    PartialPolyLine p -> draw p


data Mode = PolyLineMode | PenMode deriving (Show,Read,Eq)

switchMode :: Mode -> Mode
switchMode = \case
  PolyLineMode -> PenMode
  PenMode      -> PolyLineMode


type Color = RGB Word8


-- default color presets in goodnotes
colorPresets :: NonEmpty Color
colorPresets = NonEmpty.fromList
               [ RGB 0   0   0
               , RGB 99  99  99
               , RGB 155 155 155
               , RGB 210 210 210
               , RGB 252 252 252

               , RGB 119 41  135 -- purple
               , RGB 192 40  27 -- darkish red
               , RGB 229 95  90  -- lightish red
               , RGB 241 156 153
               , RGB 232 158 66 -- orange

               , RGB 53  121 246 -- blue
               , RGB 28  68  138 -- darkblue
               , RGB 49  113 86 -- darkgreen
               , RGB 142 196 79 -- lightgreen
               , RGB 254 255 149
               ]

defaultQuickColors :: Vector 4 Color
defaultQuickColors = Vector4 black' red blue green
  where
    black' = RGB 0 0 0
    red = RGB 192 40  27
    blue = RGB 53  121 246
    green = RGB 49  113 86


selectedColor :: Color
selectedColor = RGB 205 205 205

backgroundColor :: Color
backgroundColor = RGB 246 246 246


data Thickness = Thin | Normal | Thick deriving (Show,Read,Eq,Ord)

toInt :: Thickness -> MisoString
toInt = ms @Int . \case
  Thin   -> 1
  Normal -> 2
  Thick  -> 3

data PolyAttributes = PolyAttributes { _color      :: {-# UNPACK#-}  !Color
                                     , _colorIndex :: {-# UNPACK #-} !Int
                                     , _thickness  :: {-# UNPACK #-} !Thickness
                                     } deriving (Show,Eq)
makeLenses ''PolyAttributes

instance Default PolyAttributes where
  def = PolyAttributes { _color      = RGB 0   0   0
                       , _colorIndex = 0
                       , _thickness  = Normal
                       }

data Model = Model { _canvas       :: Canvas R
                   , _polyLines    :: IntMap.IntMap (PolyLine' R :+ PolyAttributes)
                   , _currentPoly  :: Maybe (PartialPolyLine R)
                   , _mode         :: {-# UNPACK #-} !Mode
                   , _currentAttrs :: !PolyAttributes
                   , _quickColors  :: !(Vector 4 Color)
                   } deriving (Eq,Show)
makeLenses ''Model



--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

instance Default (Point 2 R :+ Int) where
  def = origin :+ 0

----------------------------------------

initialModel :: Model
initialModel = Model { _canvas       = blankCanvas 400 400
                     , _polyLines    = mempty
                     , _currentPoly  = Nothing
                     , _mode         = PenMode
                     , _currentAttrs = def
                     , _quickColors  = defaultQuickColors
                     }

--------------------------------------------------------------------------------


data Action = Id
            | CanvasAction !Canvas.InternalCanvasAction
            | WindowResize !(Vector 2 Int)
            | SwitchMode
            | CanvasClicked
            | CanvasRightClicked
            | StartMouseDown
            | StopMouseDown
            | StartTouch
            | TouchMove
            | EndTouch
            | MouseMove
            | AddPoint
            | AddPoly
            | SelectColor {-# UNPACK #-}!Int {-# UNPACK #-}!Color
            deriving (Show,Eq)

windowDeltas :: Vector 2 Int
windowDeltas = Vector2 50 200





updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                 -> noEff m
    CanvasAction ca    -> traceShow ("canvas act: ",ca) $
      m&canvas %%~ flip Canvas.handleInternalCanvasAction ca
    WindowResize dims -> noEff $ m&canvas.dimensions .~ (dims ^-^ windowDeltas)
    SwitchMode         -> noEff $ m&mode %~ switchMode
    CanvasClicked      -> case m^.mode of
                            PolyLineMode -> m <# pure AddPoint
                            PenMode      -> noEff m
    CanvasRightClicked -> case m^.mode of
                            PolyLineMode -> m <# pure AddPoly
                            PenMode      -> noEff m

    StartMouseDown     -> startMouseDown
    MouseMove          -> mouseMove
    StopMouseDown      -> stopMouseDown

    StartTouch         -> traceShow ("startTouch",m^.canvas.mouseCoordinates) startMouseDown
    TouchMove          -> traceShow ("touchMove",m^.canvas.mouseCoordinates) mouseMove
    EndTouch           -> traceShow ("endTouch",m^.canvas.mouseCoordinates) stopMouseDown

    AddPoint           -> addPoint
    AddPoly            -> addPoly
    SelectColor i c    -> let setColor a = a&color      .~ c
                                            &colorIndex .~ i
                          in noEff $ m&currentAttrs     %~ setColor
                                      &quickColors.ix i .~ c

  where
    startMouseDown = case m^.mode of
        PolyLineMode -> noEff m
        PenMode      -> noEff $ m&currentPoly .~ extend Nothing
    mouseMove      = case m^.mode of
        PolyLineMode -> noEff m
        PenMode      -> noEff $ m&currentPoly %~ \mp -> case mp of
                                                          Nothing -> Nothing
                                                          Just _  -> extend mp
    stopMouseDown  = case m^.mode of
        PolyLineMode -> noEff m
        PenMode      -> m <# pure AddPoly

    extend = extendWith (m^.canvas.mouseCoordinates)
    addPoint = noEff $ m&currentPoly %~ extend

    addPoly  = noEff $ m&polyLines   %~ insert' (extend (m^.currentPoly))
                        &currentPoly .~ Nothing

    insert' = \case
      Just (PartialPolyLine x) -> insertPoly (x :+ m^.currentAttrs)
      _                        -> id



-- | Extend the current partial polyline with the given point (if it is on the canvas.)
extendWith          :: Eq r => Maybe (Point 2 r)
                    -> Maybe (PartialPolyLine r) -> Maybe (PartialPolyLine r)
extendWith mp state = case mp of
    Nothing -> state
    Just p  -> Just $ case state of
      Nothing                   -> StartPoint p
      Just (StartPoint s)       -> PartialPolyLine . polyLineFromPoints $ s :| [p]
      Just (PartialPolyLine pl) -> PartialPolyLine $ extendPolyLine pl p

extendPolyLine      :: Eq r => PolyLine' r -> Point 2 r -> PolyLine' r
extendPolyLine pl p = pl&_PolyLineF %~ \vs@(_ :>> q) -> if p /= q then vs |>> p else vs

-- | Helper to insert a new item into an IntMap
insertPoly     :: p -> IntMap.IntMap p -> IntMap.IntMap p
insertPoly p m = let k = case IntMap.lookupMax m of
                            Nothing    -> 0
                            Just (i,_) -> succ i
                 in IntMap.insert k p m

--------------------------------------------------------------------------------

viewModel  :: Model -> View Action
viewModel m =
    div_ [ styleM_ [ "display" =: "flex"
                   , "flex-direction" =: "column"
                   ]
         ]
         [ theToolbar m
         , theCanvas m
         , iconLink
         ]

singleton :: a -> [a]
singleton = (:[])

buttonGroup         :: Foldable f
                    => f (MisoString, MisoString) -> View action
buttonGroup buttons =
    div_ [ ]
         (foldMap (singleton .  button) buttons)
  where
    button (t,icon') =
      button_ [ class_      "button"
              , Miso.title_ t
              ]
              [ icon icon' ]

theToolbar   :: Model -> View Action
theToolbar m =
      div_ [ id_    "toolBar"
           , styleM_ [ "border"  =: "1px solid black"
                     , "display" =: "flex"
                     ]
           ]
           [ tools
           , penProperties
           , colorSelectors
           ]
  where
    tools =
      div_ [ styleM_ [ "width" =: "30%"
                     , "margin" =: "auto"
                     ]
           ]
           [ buttonGroup [ ("pen", "fas fa-pencil-alt")
                         , ("poly", "fas fa-pencil-ruler")
                         , ("eraser", "fas fa-eraser")
                         ]
           ]
    penProperties =
      div_ [ styleM_ [ "width" =: "30%"
                     , "margin" =: "auto"
                     ]
           ]
           [ toolGroup [ tool "pen-width"
                       ]
           ]
    colorSelectors =
      div_ [ styleM_ [ "width" =: "30%"
                     , "margin" =: "auto"
                     ]
           ]
           [ toolGroup $ colorPickers (m^.currentAttrs.color)
                                      (m^.currentAttrs.colorIndex) (m^.quickColors)
           ]

    toolGroup = div_ [ ]

    tool t = button_ [ class_ "button" ]
                     [ text t]

--------------------------------------------------------------------------------

colorPickers            :: FoldableWithIndex Int f => Color -> Int -> f Color -> [View Action]
colorPickers selected i = ifoldMap (\j c -> [colorPickerButton (i == j && selected == c) j c])

colorPickerButton              :: Bool -> Int -> Color -> View Action
colorPickerButton selected i c =
  button_ [ styleM_ [ "background-color" =: ms c
                    , "width"            =: (ms size <> "px")
                    , "height"           =: (ms size <> "px")
                    , "display"          =: "display"
                    , "margin"           =: "3px"
                    , "cursor"           =: "pointer"
                    , "border"           =: "none"
                    , "border-radius"    =: (ms (size `div` 2) <> "px")
                    , "outline"          =: if selected then "3px solid " <> ms selectedColor
                                                        else "none"
                    ]
                  , onClick $ SelectColor i c
                  ] []
  where
    size = 30 :: Int


--------------------------------------------------------------------------------


theCanvas   :: Model -> View Action
theCanvas m =
      div_ [ id_ "canvas"
           , styleM_ [ "margin-left"  =: "auto"
                     , "margin-right" =: "auto"
                     ]
           ]
           [ either CanvasAction id <$>
             Canvas.svgCanvas_ (m^.canvas)
                               [ onClick      CanvasClicked
                               , onRightClick CanvasRightClicked
                               , onMouseDown  StartMouseDown
                               , onMouseUp    StopMouseDown
                               , onTouchStart StartTouch
                               , onTouchMove  TouchMove
                               , onTouchEnd   EndTouch
                               , onMouseMove  MouseMove
                               , styleM_ [ "border"           =: "1px solid black"
                                         , "background-color" =: "white"
                                         ]

                               ]
                               canvasBody
           -- ,  div_ []
           --         (colorPickers (m^.currentAttrs.color)
           --                       (m^.currentAttrs.colorIndex) (m^.quickColors)
           --         )
           , div_ [ onClick SwitchMode ]
                  [ text . ms . show $ m^.mode ]
           , div_ []
                  [text . ms . show $ m^.canvas.mouseCoordinates ]
           , div_ []
                  [text . ms . show $ m^.currentPoly ]
           , Html.style_ [] $ unlines'
                [ "html { overscroll-behavior: none; }" -- prevent janky scrolling on ipad
                , "html body { overflow: hidden; }"     -- more weird scrolling prevent
                , "body { background-color: " <> ms backgroundColor <> ";}"
                ]

           ]
  where
    unlines' = mconcat . List.intersperse "\n"

    canvasBody = [ g_ [] [ draw v [ stroke_        $ ms (m^.currentAttrs.color)
                                  , strokeLinecap_ "round"
                                  , strokeWidth_   $ toInt (m^.currentAttrs.thickness)
                                  ]
                         ]
                 | v <- currentPoly'
                 ]
              <> [ g_ [] [ draw p [ stroke_        $ ms (ats^.color)
                                  , strokeLinecap_ "round"
                                  , strokeWidth_   $ toInt (ats^.thickness)
                                  ]
                         -- , draw (douglasPeucker 20 p) [ stroke_ "gray"]
                         ]
                 | (_,p :+ ats) <- m^..polyLines.ifolded.withIndex ]

              -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

    currentPoly' = case m^.currentPoly of
                     Nothing     -> []
                     cp@(Just _) -> maybeToList $ extendWith (m^.canvas.mouseCoordinates) cp

--------------------------------------------------------------------------------

instance ToMisoString Word8 where
  toMisoString = toMisoString @Int . fromIntegral

--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $
         startApp $
            App { model         = initialModel
                , update        = flip updateModel
                , view          = viewModel
                , subs          = [ windowDimensionsSub WindowResize
                                  ]
                , events        = Canvas.withCanvasEvents defaultEvents
                , initialAction = Id
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


-- | Subscription that gets the window dimensions.
windowDimensionsSub   :: (Vector 2 Int -> action) -> Sub action
windowDimensionsSub f = windowCoordsSub (\(h,w) -> f $ Vector2 w h)



--------------------------------------------------------------------------------

styleM_    :: [Map.Map MisoString MisoString] -> Attribute action
styleM_ xs = style_ $ mconcat xs




-- | Produce an icon
icon    :: MisoString -> View action
icon cs = i_ [ class_ cs, textProp "aria-hidden" "true"] []

-- | Produce a linked icon
iconLink :: View action
iconLink = Miso.script_ [ src_ "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
                        , defer_ "true"
                        ] mempty
