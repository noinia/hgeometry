{-# LANGUAGE OverloadedStrings          #-}
module Svg where

import qualified Data.Map as M

import           Control.Arrow
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (MisoString, pack, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
-- import           Touch

trunc = truncate *** truncate

mainJSM :: JSM ()
mainJSM = do
    let myApp = App { model         = emptyModel
                    , update        = updateModel
                    , view          = viewModel
                    , subs          = [ mouseSub HandleMouse ]
                    , events        = M.insert (pack "mousemove") False $ defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    }
    startApp myApp

emptyModel :: Model
emptyModel = Model (0,0)

updateModel :: Action -> Model -> Effect Action Model
-- updateModel (HandleTouch (TouchEvent touch)) model =
--   model <# do
--     -- putStrLn "Touch did move"
--     -- print touch
--     return $ HandleMouse $ trunc . page $ touch
updateModel (HandleMouse newCoords) model =
  noEff model { mouseCoords = newCoords }
updateModel Id model = noEff model

data Action
  = HandleMouse (Int, Int)
  -- | HandleTouch TouchEvent
  | Id

newtype Model
  = Model
  { mouseCoords  :: (Int, Int)
  } deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel (Model (x,y)) =
  div_ [ ] [
    svg_ [ style_ $ M.fromList [ ("border-style", "solid")
                               , ("height", "700px")
                               ]
         , transform_ "scale(1,-1)"
         , width_ "auto"
       ] [
     g_ [] [
     ellipse_ [ cx_ $ ms x
              , cy_ $ ms y
              , style_ svgStyle
              , rx_ "100"
              , ry_ "100"
              ] [ ]
     ]
     , text_ [ x_ $ ms x
             , y_ $ ms y
             ] [ text $ ms $ show (x,y) ]
   ]
 ]

svgStyle :: M.Map MisoString MisoString
svgStyle =
  M.fromList [
      ("fill", "yellow")
    , ("stroke", "purple")
    , ("stroke-width", "2")
    ]
