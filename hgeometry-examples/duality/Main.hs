{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

import           Control.Lens hiding (view, element)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           GHC.TypeNats
import           HGeometry.Box
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.Intersection
import           HGeometry.Transformation
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Viewport
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates, theViewport)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

type R = RealNumber 5
type Color = MisoString


data PartialLine = PrimalStart !(Point 2 R)
                 | DualStart !(Point 2 R)
                 deriving (Show,Eq)

data Model = Model { _primalCanvas  :: Canvas R
                   , _dualCanvas    :: Canvas R
                   , _primalPoints  :: IntMap.IntMap (Point 2 R :+ Color)
                   , _primalLines   :: IntMap.IntMap (LineEQ R :+ Color)
                   , _partialLine   :: Maybe (PartialLine :+ Color)
                   } deriving (Eq)
makeLenses ''Model

--------------------------------------------------------------------------------

dualPoints :: Getter Model (IntMap.IntMap (Point 2 R :+ Color))
dualPoints = to $ over (traverse.core) dualPoint . (^.primalLines)

dualLines :: Getter Model (IntMap.IntMap (LineEQ R :+ Color))
dualLines = to $ over (traverse.core) dualLine . (^.primalPoints)

--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

----------------------------------------

initialModel :: Model
initialModel = Model canvas canvas mempty mempty Nothing
  where
    canvas = blankCanvas 576 576 & theViewport.worldToHost %~ (uniformScaling 10 |.|)
    -- scale world to host by a factor of 10

--------------------------------------------------------------------------------

data Action = Id
            | PrimalCanvasAction Canvas.InternalCanvasAction
            | DualCanvasAction Canvas.InternalCanvasAction
            | PrimalClick
            | DualClick
            deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id                    -> noEff m
    PrimalCanvasAction ca -> m&primalCanvas %%~ flip Canvas.handleInternalCanvasAction ca
    DualCanvasAction ca   -> m&dualCanvas   %%~ flip Canvas.handleInternalCanvasAction ca
    PrimalClick           -> noEff addPrimalPoint
    DualClick             -> noEff addDualPoint
  where
    color = "red"
    addPrimalPoint = case m^.primalCanvas.mouseCoordinates of
                       Nothing -> m
                       Just p  -> m&primalPoints %~ insert (p :+ color)

    addDualPoint = case m^.dualCanvas.mouseCoordinates of
                     Nothing -> m
                     Just p  -> m&primalLines %~ insert (dualLine p :+ color)

insert     :: p -> IntMap.IntMap p -> IntMap.IntMap p
insert p m = let k = case IntMap.lookupMax m of
                            Nothing    -> 0
                            Just (i,_) -> succ i
                  in IntMap.insert k p m

--------------------------------------------------------------------------------

viewModel       :: Model -> View Action
viewModel m = div_ [ ]
                   [ either PrimalCanvasAction id <$>
                     Canvas.svgCanvas_ (m^.primalCanvas)
                                       [ onClick PrimalClick
                                       , styleInline_ "border: 1px solid black"
                                       ]
                                       primalBody
                   , either DualCanvasAction id <$>
                     Canvas.svgCanvas_ (m^.dualCanvas)
                                       [ onClick DualClick
                                       , styleInline_ "border: 1px solid black"
                                       ]
                                       dualBody
                   , div_ []
                          [text . ms . show $ m^.primalCanvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.dualCanvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.primalPoints ]
                   , div_ []
                          [text . ms . show $ m^.primalLines ]
                    ]
  where
    partialPrimalLine = []
    partialDualLine = []

    primalBody = drawWorld primalPoints primalLines (m^.primalCanvas.mouseCoordinates)
    dualBody   = drawWorld dualPoints   dualLines   (m^.dualCanvas.mouseCoordinates)

    drawWorld points lines mousePos =
         [ g_ [] [ draw p [ fill_ color
                          ]
                 ]
         | p :+ color <- m^..points.folded ]
      <> [ g_ [] [ draw l [ stroke_ color
                          ]
                 ]
         | l :+ color <- m^..lines.folded ]
      -- <> [ draw p [ fill_ "blue" ]  | Just p <- [mousePos] ]

instance Drawable (LineEQ R) where
  draw l = let maxP = Point2 large large
               minP = maxP&vector %~ negated
           in case l `intersect` Rectangle minP maxP of
                Just (Line_x_Box_LineSegment s) -> draw s
                _                               -> flip g_ []

large = 100000

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
