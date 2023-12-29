{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
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
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.PolyLine.Simplification.DouglasPeucker
import           HGeometry.Sequence.NonEmpty
import           HGeometry.Vector
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso hiding (onMouseUp, onMouseDown)
import           Miso.Event.Extra
import qualified Miso.Html.Element as Html
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
import           Miso.Util ((=:))


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

-- instance ToMisoString a => ToMisoString (RGB a) where
--   toMisoString (RGB r g b) = mconcat . intersperse " " . map toMisoString $ [r,g,b]

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
backgroundColor = RGB 246 246 256


data Thickness = Thin | Normal | Thick deriving (Show,Read,Eq,Ord)

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
initialModel = Model { _canvas       = blankCanvas 300  300
                     , _polyLines    = mempty
                     , _currentPoly  = Nothing
                     , _mode         = PenMode
                     , _currentAttrs = def
                     , _quickColors  = defaultQuickColors
                     }

--------------------------------------------------------------------------------


data Action = Id
            | CanvasAction Canvas.InternalCanvasAction
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
            | SelectColor !Int !Color
            deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                 -> noEff m
    CanvasAction ca    -> m&canvas %%~ flip Canvas.handleInternalCanvasAction ca
    SwitchMode         -> noEff $ m&mode %~ switchMode
    CanvasClicked      -> case m^.mode of
                            PolyLineMode -> m <# pure AddPoint
                            PenMode      -> noEff m
    CanvasRightClicked -> case m^.mode of
                            PolyLineMode -> m <# pure AddPoly
                            PenMode      -> noEff m
    StartMouseDown     -> case m^.mode of
                            PolyLineMode -> noEff m
                            PenMode      -> noEff $ m&currentPoly .~ extend Nothing
    StopMouseDown      -> case m^.mode of
                            PolyLineMode -> noEff m
                            PenMode      -> m <# pure AddPoly

    StartTouch         -> m <# pure StartMouseDown
    TouchMove          -> m <# pure MouseMove
    EndTouch           -> m <# pure StopMouseDown

    MouseMove          -> case m^.mode of
                            PolyLineMode -> noEff m
                            PenMode      -> mouseMoveAction
    AddPoint           -> addPoint
    AddPoly            -> addPoly
    SelectColor i c    -> let setColor a = a&color      .~ c
                                            &colorIndex .~ i
                          in noEff $ m&currentAttrs     %~ setColor
                                      &quickColors.ix i .~ c

  where
    extend = extendWith (m^.canvas.mouseCoordinates)
    addPoint = noEff $ m&currentPoly %~ extend

    addPoly  = noEff $ m&polyLines   %~ insert' (extend (m^.currentPoly))
                        &currentPoly .~ Nothing

    insert' = \case
      Just (PartialPolyLine x) -> insertPoly (x :+ m^.currentAttrs)
      _                        -> id

    mouseMoveAction = noEff $ m&currentPoly %~ \mp -> case mp of
                                                        Nothing -> Nothing
                                                        Just _  -> extend mp

-- | Extend the current partial polyline with the given point (if it is on the canvas.)
extendWith          :: Eq r => Maybe (Point 2 r)
                    -> Maybe (PartialPolyLine r) -> Maybe (PartialPolyLine r)
extendWith mp state = case mp of
    Nothing -> state
    Just p  -> Just $ case state of
      Nothing                   -> StartPoint p
      Just (StartPoint s)       -> PartialPolyLine . polylineFromPoints $ s :| [p]
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

viewModel       :: Model -> View Action
viewModel m = div_ [ ]
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
                                       , styleInline_ "border: 1px solid black"
                                       ]
                                       canvasBody
                   ,  div_ []
                           (colorPickers (m^.currentAttrs.color)
                                         (m^.currentAttrs.colorIndex) (m^.quickColors)
                           )
                   , div_ [ onClick SwitchMode ]
                          [ text . ms . show $ m^.mode ]
                   , div_ []
                          [text . ms . show $ m^.canvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.polyLines ]
                   , Html.style_ [] $ unlines'
                        [ "html { overscroll-behavior: none; }"
                        , "html body { overflow: hidden; }"
                        ]
                        -- these two lines prevent weird janky scrolling on ipad
                   ]
  where
    unlines' = mconcat . List.intersperse "\n"

    canvasBody = [ g_ [] [ draw v [ stroke_        "red"
                                  , strokeLinecap_ "round"
                                  , strokeWidth_   $ toInt (m^.currentAttrs.thickness)
                                  ]
                         ]
                 | v <- currentPoly'
                 ]
              <> [ g_ [] [ draw p [ stroke_        $ "rgb(" <> ms (ats^.color) <> ")"
                                  , strokeLinecap_ "round"
                                  , strokeWidth_   $ toInt (ats^.thickness)
                                  ]
                         , draw (douglasPeucker 20 p) [ stroke_ "gray"]
                         ]
                 | (_,p :+ ats) <- m^..polyLines.ifolded.withIndex ]

              -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

    currentPoly' = case m^.currentPoly of
                     Nothing     -> []
                     cp@(Just _) -> maybeToList $ extendWith (m^.canvas.mouseCoordinates) cp


colorPickers            :: FoldableWithIndex Int f => Color -> Int -> f Color -> [View Action]
colorPickers selected i = ifoldMap (\j c -> [colorPickerButton (i == j && selected == c) j c])

colorPickerButton              :: Bool -> Int -> Color -> View Action
colorPickerButton selected i c =
    button_ [ style_ $ mconcat
              [ "background-color" =: rgb c
              , "width"            =: size
              , "height"           =: size
              , "display"          =: "display"
              , "padding"          =: "15px 32px"
              , "text-align"       =: "center"
              , "text-decoration"  =: "none"
              , "display"          =: "inline-block"
              , "font-size"        =: "16px"
              , "margin"           =: "4px 2px"
              , "cursor"           =: "pointer"
              , "border"           =: if selected then "3px solid " <> rgb selectedColor
                                                  else "none"
              ]
            , onClick $ SelectColor i c
            ] []
  where
    size = "30"

instance ToMisoString Word8 where
  toMisoString = toMisoString @Int . fromIntegral

rgb             :: ToMisoString a => RGB a -> MisoString
rgb (RGB r g b) = "rgb(" <> ms r <> "," <> ms g <> "," <> ms b <> ")"

-- rgb             :: ToMisoString a => RGB a -> MisoString
-- rgb (RGB r g b) = "rgb(" <> ms r <> "," <> ms g <> "," <> ms b <> ")"


--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $
         startApp $
            App { model         = initialModel
                , update        = flip updateModel
                , view          = viewModel
                , subs          = mempty
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




--------------------------------------------------------------------------------
