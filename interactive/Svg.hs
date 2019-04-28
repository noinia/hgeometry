{-# LANGUAGE OverloadedStrings          #-}
module Svg where

import           Control.Lens hiding (view, element)
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import qualified Data.Map as M
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (MisoString, pack, ms)
import           Miso.Svg hiding (height_, id_, style_, width_)
-- import           Touch

main :: IO ()
main = JSaddle.run 8080 $ mainJSM


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
emptyModel = Model $ Canvas (Point2 512 288) (Vector2 1024 576) 1

updateModel :: Action -> Model -> Effect Action Model
-- updateModel (HandleTouch (TouchEvent touch)) model =
--   model <# do
--     -- putStrLn "Touch did move"
--     -- print touch
--     return $ HandleMouse $ trunc . page $ touch
updateModel (HandleMouse (mx,my)) (Model cv) =
    -- noEff (Model cv)
    -- noEff . Model $ cv&center .~ (fromIntegral <$> p)
    -- noEff . Model $ cv&scale .~ (1 + (576 / fromIntegral my))
    noEff . Model $ cv&scale .~ (1 + (fromIntegral my / 576.0))
  where
    p = Point2 mx ((cv^.dimensions.element (C :: C 1)) - my)
updateModel Id model' = noEff model'


data Action = HandleMouse (Int, Int)
            | Id
             -- | HandleTouch TouchEvent

newtype Model = Model { myCanvas :: Canvas Double
                      } deriving (Show, Eq)


data Canvas r = Canvas { _center     :: Point 2 r
                       , _dimensions :: Vector 2 Int
                       , _scale      :: r
                       } deriving (Show,Eq)

center     :: Lens' (Canvas r) (Point 2 r)
center     = lens _center     (\cv c -> cv { _center     = c } )

dimensions :: Lens' (Canvas r) (Vector 2 Int)
dimensions = lens _dimensions (\cv c -> cv { _dimensions = c } )

scale      :: Lens' (Canvas r) r
scale      = lens _scale      (\cv c -> cv { _scale      = c } )


renderCanvas           :: (RealFrac r, Show r)
                       =>  Canvas r -> [Attribute action] -> [View action] -> View action
renderCanvas cv ats vs = svg_ [ width_   . ms $ w
                              , height_  . ms $ h
                              , viewBox_      $ outerVB
                              , style_ $ M.fromList [ ("border-style", "solid") ]
                              ]
                              [ g_ [ transform_ "scale(1,-1)" ]
                                   [ svg_ ([ width_ "100%"
                                           , height_ "100%"
                                           , viewBox_ $ innerVB
                                           ] <> ats) vs
                                   ]
                              ]
  where
    dims@(Vector2 w h) = cv^.dimensions
    Point2 cx cy       = round <$> cv^.center

    Vector2 vw vh = round  <$> (1 / cv^.scale) *^ (fromIntegral <$> dims)


    toVB = mconcat @MisoString . List.intersperse " " . map ms
    outerVB = toVB [0, (-1) * h, w, h]
      -- role of the outer viewBox is to flip the coordinate system s.t. the origin
      -- is in the bottom left rather than the top-left

    innerVB = toVB [(cx - (vw `div` 2)), (cy - (vh `div` 2)), vw, vh]
    -- innerVB = toVB [0, 0, vw, vh]

-- | rescale the text
text'_              :: (Int,Int) -- ^ position where to draw
                    -> [Attribute action]
                    -> [View action] -> View action
text'_ (x,y) ats vs = g_ [ transform_ $ mconcat [ "translate("
                                                , ms x
                                                , ", "
                                                , ms y
                                                , ")scale(1,-1)"
                                                ]
                         ] [ text_ ats vs  ]


viewModel            :: Model -> View Action
viewModel (Model cv) = div_ [ ] [
      renderCanvas cv
                              [
                              ]
                              [ ellipse_ [ cx_ "512", cy_ "288", rx_ "50", ry_ "50"
                                         , fill_ "red"
                                         ] []
                              -- , rect_ [width_"100000", height_ "2000" , x_ "-1000", y_ "-1000"
                              --         , fill_ "blue"
                              --         ] []
                              , text'_ (100,20) [] [text "Woei!"]
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

svgStyle :: M.Map MisoString MisoString
svgStyle =
  M.fromList [
      ("fill", "yellow")
    , ("stroke", "purple")
    , ("stroke-width", "2")
    ]
