module Miso.Component.Mouse where

import Data.Geometry.Point
import Miso hiding (update, view)

newtype MousePosition = MousePosition (Maybe (Point 2 Int)) deriving (Show,Eq)

data MouseAction = MouseMove (Int,Int)
                 | MouseLeave
                 deriving (Show,Eq)

update   :: MousePosition -> Effect action MousePosition
update m = \case
  MouseMove (x,y) -> let p = Point2 x y in noEff $ MousePosition (Just p)
  MouseLeave      -> noEff $ MousePosition Nothing


viewA   :: (MouseAction -> action) -> Attribute action
viewA f = undefined
