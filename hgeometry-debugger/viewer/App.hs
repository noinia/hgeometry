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
import           Miso.Svg hiding (style_, script_)
import           Miso.Svg.Property
import           Miso.Html.Element hiding (style_)
import           Miso.Html.Property
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

data Item = Item { _content   :: String
                 , _drawing   :: Drawing
                 , _isVisible :: Bool
                 }
            deriving (Show,Eq)

makeLenses ''Item


data Model = Model { _drawings :: Map LayerName (Seq Item)
                   }
           deriving (Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model dummy -- mempty

dummy = Map.fromList [ ("myLayer", Seq.fromList [Item "foo" "foodrawing" True])
                     , ("bar", Seq.fromList [ Item "bar" "bar" True
                                            , Item "baz" "bazz" False
                                            ])
                     ]


--------------------------------------------------------------------------------

data Action


updateModel   :: Action -> Effect parent Model Action
updateModel _ = pure ()

--------------------------------------------------------------------------------0

viewModel       :: Model -> View Model Action
viewModel model = div_ []
                       [ svg_ [ width_  "1024px"
                              , height_ "700px"
                              , style_ [border "1px solid black"]
                              ]
                              content
                       , dl_ [ class_ "max-w-md text-heading divide-y divide-default"]
                             drawingItems
                       , flowBite
                       ]
  where
    content = [

              ]
    drawingItems = [ div_ [class_ "flex flex-col pb-3"]
                          [ dt_ [ class_ "mb-1 text-body"
                                ]
                                [ text $ ms layer
                                ]
                          , dd_ [ class_ "text-lg font-medium"]
                                [ text $ ms $ show items ]
                          ]
                   | (layer,items) <- model^..drawings.ifolded.withIndex
                   ]



flowBite = script_ [src_ "https://cdn.jsdelivr.net/npm/flowbite@4.0.1/dist/flowbite.min.js"
                   ] ""



-- <dl class="max-w-md text-heading divide-y divide-default">
--     <div class="flex flex-col pb-3">
--         <dt class="mb-1 text-body">Email address</dt>
--         <dd class="text-lg font-medium">yourname@flowbite.com</dd>
--     </div>
--     <div class="flex flex-col py-3">
--         <dt class="mb-1 text-body">Home address</dt>
--         <dd class="text-lg font-medium">92 Miles Drive, Newark, NJ 07103, California, USA</dd>
--     </div>
--     <div class="flex flex-col pt-3">
--         <dt class="mb-1 text-body">Phone number</dt>
--         <dd class="text-lg font-medium">+00 123 456 789 / +12 345 678</dd>
--     </div>
-- </dl>



--------------------------------------------------------------------------------

main :: IO ()
main = startApp defaultEvents $ component initialModel updateModel viewModel
