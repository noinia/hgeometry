{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module App(main) where

import           Control.Lens hiding (view, element)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
-- import           GHC.TypeNats
-- import           HGeometry.ConvexHull.GrahamScan
-- import           HGeometry.Ext
-- import           HGeometry.Miso.OrphanInstances ()
-- import           HGeometry.Miso.Svg
-- import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
-- import qualified HGeometry.Miso.Svg.Canvas as Canvas
-- import           HGeometry.Number.Real.Rational
-- import           HGeometry.Point
-- import           HGeometry.Polygon.Convex
import           Miso hiding (text_)
import           Miso.String (ToMisoString(..))
import           Miso.CSS (style_, border)
import           Miso.Svg hiding (style_)
import           Miso.Svg.Property
import           Miso.Html.Element hiding (style_)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import           Data.Text (Text)

--------------------------------------------------------------------------------


type LayerName = String -- TODO: fix
type Drawing = Text -- TOEDO: fix

--------------------------------------------------------------------------------

type R = Double

data Model = Model { _drawings :: Map LayerName (Seq (String, Drawing))
                   }
           deriving (Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model mempty

--------------------------------------------------------------------------------

data Action


updateModel   :: Action -> Effect parent Model Action
updateModel _ = pure ()

--------------------------------------------------------------------------------0

viewModel       :: Model -> View Model Action
viewModel model = div_ []
                       [text "the viewer"]

--------------------------------------------------------------------------------

main :: IO ()
main = startApp defaultEvents $ component initialModel updateModel viewModel
