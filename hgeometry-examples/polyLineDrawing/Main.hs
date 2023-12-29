{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main (main) where

import           Control.Monad.IO.Class

import           Control.Lens hiding (view, element)
import           Data.Default.Class
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import qualified Data.Sequence as Sequence
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
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso hiding (onMouseUp, onMouseDown)
import           Miso.Event.Extra
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

data Model = Model { _canvas      :: Canvas R
                   , _polyLines   :: IntMap.IntMap (PolyLine' R)
                   , _currentPoly :: Maybe (PartialPolyLine R)
                   , _mode        :: Mode
                   } deriving (Eq)
makeLenses ''Model

--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

instance Default (Point 2 R :+ Int) where
  def = origin :+ 0

----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 300  300) mempty Nothing PenMode


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

    StartTouch         -> traceShow "touchstart" $ m <# pure StartMouseDown
    TouchMove          -> traceShow "touchmove"  $ m <# pure MouseMove
    EndTouch           -> traceShow "touchend"   $ m <# pure StopMouseDown

    MouseMove          -> case m^.mode of
                            PolyLineMode -> noEff m
                            PenMode      -> mouseMoveAction
    AddPoint           -> addPoint
    AddPoly            -> addPoly
  where
    extend = extendWith (m^.canvas.mouseCoordinates)
    addPoint = noEff $ m&currentPoly %~ extend

    addPoly  = noEff $ m&polyLines   %~ insert' (extend (m^.currentPoly))
                        &currentPoly .~ Nothing

    insert' = \case
      Just (PartialPolyLine x) -> insertPoly x
      _                        -> id

    mouseMoveAction = noEff $ m&currentPoly %~ \mp -> case mp of
                                                        Nothing -> Nothing
                                                        Just _  -> extend mp

    -- setMouseCoords (TouchEvent t) = m
      -- let (x,y) = bimap trunc trunc $ page t
      --                               in m&canvas.mousePosition ?~ p


-- | Extend the current partial polyline with the given point (if it is on the canvas.)
extendWith          :: Maybe (Point 2 r)
                    -> Maybe (PartialPolyLine r) -> Maybe (PartialPolyLine r)
extendWith mp state = case mp of
    Nothing -> state
    Just p  -> Just $ case state of
      Nothing                   -> StartPoint p
      Just (StartPoint s)       -> PartialPolyLine . polylineFromPoints $ s :| [p]
      Just (PartialPolyLine pl) -> PartialPolyLine $ extendPolyLine pl p

extendPolyLine      :: PolyLine' r -> Point 2 r -> PolyLine' r
extendPolyLine pl p = pl&_PolyLineF %~ (|>> p)

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
                   , div_ [ onClick SwitchMode ]
                          [ text . ms . show $ m^.mode ]
                   , div_ []
                          [text . ms . show $ m^.canvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.polyLines ]
                    ]
  where
    canvasBody = [ g_ [] [ draw v [ stroke_        "red"
                                  , strokeLinecap_ "round"
                                  ]
                         ]
                 | v <- currentPoly'
                 ]
              <> [
                 ]
              <> [ g_ [] [ draw p [ stroke_        "black"
                                  , strokeLinecap_ "round"
                                  ]
                         , draw (douglasPeucker 20 p) [ stroke_ "gray"]
                         ]
                 | (_,p) <- m^..polyLines.ifolded.withIndex ]
              -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

    currentPoly' = case m^.currentPoly of
                     Nothing     -> []
                     cp@(Just _) -> maybeToList $ extendWith (m^.canvas.mouseCoordinates) cp

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
