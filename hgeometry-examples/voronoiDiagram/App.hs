{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module App(main) where

import           Control.Monad.State.Class
import           Control.Lens hiding (view, element)
import           Data.Foldable (toList)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           GHC.TypeNats
import           HGeometry.Ext
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.VoronoiDiagram
import           Miso hiding (text_)
import           Miso.String (ToMisoString(..))
import           Miso.CSS (style_, border, backgroundColor)
import           Miso.Svg hiding (style_)
import           Miso.Svg.Property
import           Miso.Html hiding (style_)
import           Miso.CSS.Color as Miso
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.NonEmpty as NonEmptyV


--------------------------------------------------------------------------------

type R = RealNumber 5




data Model = Model { _canvas         :: Canvas R
                   , _points         :: IntMap.IntMap (Point 2 R :+ Color)
                   , _diagram        :: Maybe (Set.Set (Point 2 R))
                   , _currentColorIx :: !Int
                   } deriving (Eq)
makeLenses ''Model


-- | Take the current color
takeCurrentColor :: MonadState Model m => m Color
takeCurrentColor = currentColorIx %%= \i -> ( colorPresets NonEmptyV.! i
                                            , (i + 1) `mod` length colorPresets
                                            )

--------------------------------------------------------------------------------

instance KnownNat p => ToMisoString (RealNumber p) where
  toMisoString = toMisoString . toFixed

----------------------------------------

initialModel :: Model
initialModel = Model (blankCanvas 1024  576) mempty Nothing 0



--------------------------------------------------------------------------------

data Action = CanvasAction Canvas.InternalCanvasAction
            | AddPoint
            deriving (Show,Eq)


updateModel :: Action -> Effect parent Model Action
updateModel = \case
    CanvasAction ca  -> zoom canvas $ wrap Canvas.handleInternalCanvasAction ca
    AddPoint         -> addPoint
  where
    addPoint = do c <- takeCurrentColor
                  modify $ \m ->
                    let m' = case m^.canvas.mouseCoordinates of
                               Nothing -> m
                               Just p  -> m&points %~ insertPoint (p :+ c)
                    in recomputeDiagram m'

recomputeDiagram   :: Model -> Model
recomputeDiagram m
  | m^.points.to length <= 2  = m&diagram .~ Nothing
  | otherwise                 = let pts = NonEmpty.nonEmpty
                                          [ p :+ i | (i,p) <- IntMap.assocs (m^.points)]
                                in m&diagram .~ fmap voronoiVertices pts

insertPoint     :: p -> IntMap.IntMap p -> IntMap.IntMap p
insertPoint p m = let k = case IntMap.lookupMax m of
                            Nothing    -> 0
                            Just (i,_) -> succ i
                  in IntMap.insert k p m

--------------------------------------------------------------------------------

viewModel       :: Model -> View Model Action
viewModel m = div_ [ ]
                   [ either CanvasAction id <$>
                     Canvas.svgCanvas_ (m^.canvas)
                                       [ onClick AddPoint
                                       , style_ [border "1px solid black"]
                                       ]
                                       canvasBody
                   , div_ [ onClick AddPoint
                          , style_ [backgroundColor currentColor ]
                          ]
                          [ text "add point" ]
                   , div_ []
                          [text . ms . show $ m^.canvas.mouseCoordinates ]
                   , div_ []
                          [text . ms . show $ m^.points ]
                    ]

  where
    currentColor = colorPresets NonEmptyV.! (m^.currentColorIx)
    canvasBody = [ g_ [] [ draw v [ fill_ "red"
                                  ]
                         ]
                 | v <- m^..diagram.folded.to toList.traverse ]
              <> [ g_ [] [ draw p [ fill_ (renderColor c)
                                  ]
                         , textAt p [] (ms i)
                         ]
                 | (i,p :+ c) <- m^..points.ifolded.withIndex ]
              -- <> [ draw p [ fill_ "blue" ]  | Just p <- [m^.canvas.mouseCoordinates] ]

--------------------------------------------------------------------------------


main :: IO ()
main = startApp (Canvas.withCanvasEvents defaultEvents) $
         component initialModel updateModel viewModel

textAt                    :: ToMisoString r
                          => Point 2 r
                          -> [Attribute action] -> MisoString -> View model action
textAt (Point2 x y) ats t = text_ ([ x_ $ ms x
                                   , y_ $ ms y
                                   ] <> ats
                                  ) [text t]

wrap       :: (model -> action -> Effect parent model action') -> action
           -> Effect parent model action'
wrap f act = get >>= flip f act



--------------------------------------------------------------------------------


-- | default color presets in goodnotes
colorPresets :: NonEmptyV.NonEmptyVector Color
colorPresets = NonEmptyV.unsafeFromList
               [ Miso.rgb 0   0   0
               , darkishGrey
               , mediumGrey
               , lightGrey
               , Miso.rgb 252 252 252

               , Miso.rgb 119 41  135 -- purple
               , Miso.rgb 192 40  27 -- darkish red
               , Miso.rgb 229 95  90  -- lightish red
               , Miso.rgb 241 156 153
               , Miso.rgb 232 158 66 -- orange

               , myBlue
               , Miso.rgb 28  68  138 -- darkblue
               , Miso.rgb 49  113 86 -- darkgreen
               , Miso.rgb 142 196 79 -- lightgreen
               , Miso.rgb 254 255 149
               ]

darkishGrey :: Color
darkishGrey = Miso.rgb 99  99  99

mediumGrey :: Color
mediumGrey = Miso.rgb 155 155 155

lightGrey :: Color
lightGrey = Miso.rgb 210 210 210


-- | My color blue
myBlue :: Color
myBlue = Miso.rgb 53  121 246 -- blue


instance Ord Color where
  c `compare` c' = renderColor c `compare` renderColor c'
  -- this is a terrible comparing instance, but whatever
