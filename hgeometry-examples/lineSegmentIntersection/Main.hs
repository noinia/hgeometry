{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where

import           Control.Lens hiding (view, element)
import qualified Data.IntMap as IntMap
import           GHC.TypeNats
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.LineSegment.Intersection.BentleyOttmann ( Intersections
                                                                   , intersectionPoints
                                                                   )
import qualified HGeometry.LineSegment.Intersection.BentleyOttmann as BO
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           Language.Javascript.JSaddle (JSM)
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (MisoString, ToMisoString(..), ms)
import           Miso.Svg hiding (height_, id_, style_, width_)

--------------------------------------------------------------------------------

type R = RealNumber 5

data Model = Model { _canvas        :: Canvas R
                   , _segments      :: IntMap.IntMap (ClosedLineSegment (Point 2 R))
                   , _intersections :: Intersections R (ClosedLineSegment (Point 2 R) :+ Int)
                   , _partialSeg    :: Maybe (Point 2 R)
                     -- ^ We may have started to draw a segment.
                   } deriving (Eq)
makeLenses ''Model


--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 1024  576) mempty mempty Nothing

--------------------------------------------------------------------------------

data Action = Id
            | CanvasAction Canvas.InternalCanvasAction
            | AddPoint
            deriving (Show,Eq)


updateModel   :: Model -> Action -> Effect Action Model
updateModel m = \case
    Id               -> noEff m
    CanvasAction ca  -> m&canvas %%~ flip Canvas.handleInternalCanvasAction ca
    AddPoint         -> addPoint
  where
    addPoint = noEff $ case m^.partialSeg of
      Nothing -> m&partialSeg .~ m^.canvas.mouseCoordinates
      Just p  -> case m^.canvas.mouseCoordinates of
                   Nothing -> m
                   Just q  -> recomputeIntersections $
                                m&segments   %~ insertSegment (ClosedLineSegment p q)
                                 &partialSeg .~ Nothing

recomputeIntersections   :: Model -> Model
recomputeIntersections m = let segs = [ s :+ i | (i,s) <- IntMap.assocs (m^.segments)]
                           in m&intersections .~ BO.intersections segs

insertSegment     :: a -> IntMap.IntMap a -> IntMap.IntMap a
insertSegment s m = let k = case IntMap.lookupMax m of
                            Nothing    -> 0
                            Just (i,_) -> succ i
                  in IntMap.insert k s m

--------------------------------------------------------------------------------

viewModel       :: Model -> View Action
viewModel m = div_ [ ]
                   [ either CanvasAction id <$>
                     Canvas.svgCanvas_ (m^.canvas)
                                       [ onClick AddPoint
                                       , id_ "mySvg"
                                       , styleInline_ "border: 1px solid black"
                                       ]
                                       canvasBody
                   , div_ [ onClick AddPoint ]
                          [text $ "add point" ]
                   , div_ []
                          [text . ms . show $ m^.canvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.segments ]
                   -- , div_ []
                   --        [ div_ [] ["selected: "]
                   --        , text . ms . show $ m^.selected
                   --        ]
                    ]
  where
    canvasBody = [ draw s [ stroke_ "red" ]
                 | Just s <- partialSegment (m^.partialSeg)
                                            (m^.canvas.mouseCoordinates) ]
              <> [ g_ [] [ draw s [ stroke_ "black"]
                         , textAt (s^.start) [] (ms i)
                         ]
                 | (i,s) <- m^..segments.ifolded.withIndex ]
              <> [ g_ [] [ draw p [ fill_ "red"]
                         ]
                 | p <- m^..intersections.to intersectionPoints.folded ]
              -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

    partialSegment p q = [ClosedLineSegment <$> p <*> q ]

--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $ mainJSM


mainJSM :: JSM ()
mainJSM = do
    let myApp = defaultComponent initialModel
                                 (flip updateModel)
                                 viewModel
                    { subs          = mempty
                    , events        = Canvas.withCanvasEvents defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    , logLevel      = Off
                    }
    startComponent myApp

textAt                    :: ToMisoString r
                          => Point 2 r
                          -> [Attribute action] -> MisoString -> View action
textAt (Point2 x y) ats t = text_ ([ x_ $ ms x
                                   , y_ $ ms y
                                   ] <> ats
                                  ) [text t]
