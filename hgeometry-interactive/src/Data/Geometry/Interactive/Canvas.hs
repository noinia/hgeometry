module Data.Geometry.Interactive.Canvas where

import Data.Ext
import           Control.Lens hiding (view, element)
import           Data.Geometry.Point
import           Data.Geometry.Svg.MathCoordinateSystem()
import           Data.Geometry.Interactive.StaticCanvas
import           Miso
import           Miso.String (MisoString, ToMisoString, ms)
import qualified Miso.Svg as Svg


--------------------------------------------------------------------------------

-- * Model

data ICanvas r = ICanvas { _canvas        :: Canvas r
                         , _mousePosition :: Maybe (Point 2 r)
                         }

-- * Controller


data CanvasAction = HandleMouse (Int, Int)
                  | Id
                  | DoStuff String
               -- | HandleTouch TouchEvent


update                          :: Num r => CanvasAction -> Canvas r
                                      -> Effect CanvasAction (Canvas r)
update (HandleMouse mp) cv = noEff $ cv&center .~ realWorldCoordinates cv mp
    -- noEff . Model $ cv&scale .~ (1 + (fromIntegral my / 576.0))
    -- TODO: only change the center when we are panning
update Id                    cv = noEff cv
update (DoStuff s)           cv = noEff cv -- <# do putStrLn s ; pure Id

-- updateModel (HandleTouch (TouchEvent touch)) model =
--   model <# do
--     -- putStrLn "Touch did move"
--     -- print touch
--     return $ HandleMouse $ trunc . page $ touch



-- * View

view            :: ICanvas r -> View action
view cv = div_ [ ] [
      renderCanvas cv
                              [
                              ]
                              [ ellipse_ [ cx_ "512", cy_ "288", rx_ "50", ry_ "50"
                                         , fill_ "red"
                                         , onClick $ DoStuff "woei"
                                         ] []
                              -- , rect_ [width_"100000", height_ "2000" , x_ "-1000", y_ "-1000"
                              --         , fill_ "blue"
                              --         ] []
                              , textAt_ @Int (100,20) [] [text "Woei!"]
                              ]
   ]
    -- svg'_ [
    --       ]
    --       [ ellipse_ [ cx_ $ ms x
    --                  , cy_ $ ms y
    --                  , style_ svgStyle
    --                  , rx_ "100"
    --                  , ry_ "100"
    --                  ] [ ]
    --       , text'_ (x,y) [] [text $ ms $ show (x,y) ]
    --       ]
