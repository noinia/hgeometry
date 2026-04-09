{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where


import Miso
import Control.Lens hiding (view, element)
import Data.Foldable (toList)
import HGeometry.Number.Real.Rational
import HGeometry
import HGeometry.Ext
import Miso.Html
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas

--------------------------------------------------------------------------------

type R = RealNumber 5


data Model = Model { _triangeles :: [Triangle (Point 3 R) :+ ()]
                   }
             deriving stock (Eq)

makeLenses ''Model


initialModel = Model []

--------------------------------------------------------------------------------

data Action = Action ()

--------------------------------------------------------------------------------

main :: IO ()
main = startApp (Canvas.withCanvasEvents defaultEvents) $
         Miso.component initialModel (wrap updateModel) viewModel

wrap       :: (model -> action -> Effect parent model action') -> action
           -> Effect parent model action'
wrap f act = get >>= flip f act

--------------------------------------------------------------------------------

updateModel     :: Model -> Action -> Effect parent Model Action
updateModel _ _ = pure ()

--------------------------------------------------------------------------------

viewModel       :: Model -> View Model Action
viewModel m = div_ [ ] [ ]
