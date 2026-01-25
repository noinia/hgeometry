{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Main(main) where


import Miso
import Control.Lens hiding (view, element)
import Data.Foldable (toList)
import HGeometry.Number.Real.Rational
import HGeometry
import HGeometry.Ext
import Language.Javascript.JSaddle.Warp qualified as JSaddle
import Miso.Html

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
main = JSaddle.run 8080 $
         startComponent defaultEvents $
            Component
                { model         = initialModel
                , update        = updateModel
                , view          = viewModel
                , subs          = mempty
                , styles        = []
                , initialAction = Nothing
                , mountPoint    = Nothing
                , logLevel      = Off
                }

--------------------------------------------------------------------------------

-- updateModel :: Action -> Effect parent Model Action
updateModel _ = pure ()

--------------------------------------------------------------------------------

viewModel       :: Model -> View Model Action
viewModel m = div_ [ ] [ ]
