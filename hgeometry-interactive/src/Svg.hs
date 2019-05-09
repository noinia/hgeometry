{-# LANGUAGE OverloadedStrings          #-}
module Svg where

import           Control.Lens hiding (view, element)
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import qualified Data.Map as M
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso
import           Miso.String (MisoString, ToMisoString, ms)
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
                    , events        = M.insert "mousemove" False $ defaultEvents
                    , initialAction = Id
                    , mountPoint    = Nothing
                    }
    startApp myApp

--------------------------------------------------------------------------------

type Model = Canvas Double


emptyModel :: Model
emptyModel = blankCanvas

type Action = CanvasAction

updateModel :: Action -> Model -> Effect Action Model
updateModel = updateCanvas

--------------------------------------------------------------------------------

-- | Svg Canvas that has a "proper" Coordinate system whose origin is in the bottom left.
data Canvas r = Canvas { _dimensions :: Vector 2 Int
                         -- ^ dimensions (width,height) of the canvas
                       , _center     :: Point 2 r
                         -- ^ the center point (in world coordinates)
                         -- of the viewport of the canvas.
                       , _zoomLevel  :: r
                         -- ^ determines the zoomlevel of the
                         -- canvas. At zoomlevel z the width and
                         -- height (in terms of world coordinates)
                         -- that we can see are z*dimensions
                       } deriving (Show,Eq)

center     :: Lens' (Canvas r) (Point 2 r)
center     = lens _center     (\cv c -> cv { _center     = c } )

dimensions :: Lens' (Canvas r) (Vector 2 Int)
dimensions = lens _dimensions (\cv c -> cv { _dimensions = c } )

zoomLevel  :: Lens' (Canvas r) r
zoomLevel  = lens _zoomLevel      (\cv c -> cv { _zoomLevel      = c } )

----------------------------------------

blankCanvas :: Num r => Canvas r
blankCanvas = Canvas (Vector2 1024 576) (Point2 512 288) 1

-- | Draws the actual canvas
renderCanvas           :: (RealFrac r, ToSvgCoordinate r)
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

    Vector2 vw vh = round  <$> (1 / cv^.zoomLevel) *^ (fromIntegral <$> dims)


    toVB = mconcat @MisoString . List.intersperse " " . map ms
    outerVB = toVB [0, (-1) * h, w, h]
      -- role of the outer viewBox is to flip the coordinate system s.t. the origin
      -- is in the bottom left rather than the top-left

    innerVB = toVB [(cx - (vw `div` 2)), (cy - (vh `div` 2)), vw, vh]


type ToSvgCoordinate = ToMisoString

-- | To be used instead of the text_ combinator in Miso
text'_              :: ToSvgCoordinate r
                    => (r,r) -- ^ position where to draw (in world coordinates)
                    -> [Attribute action]
                    -> [View action] -> View action
text'_ (x,y) ats vs = g_ [ transform_ $ mconcat [ "translate("
                                                , ms x
                                                , ", "
                                                , ms y
                                                , ")scale(1,-1)"
                                                ]
                         ] [ text_ ats vs  ]


-- | Computes the mouse position in terms of real world coordinates.
-- pre: the coordinates given lie on the canvas
realWorldCoordinates          :: Num r => Canvas r -> (Int,Int) -> Point 2 r
realWorldCoordinates cv (x,y) = fromIntegral
                      <$> Point2 x ((cv^.dimensions.element (C @ 1)) - y)

--------------------------------------------------------------------------------



data InteractiveCanvas r = InteractiveCanvas { _canvas :: Canvas r
                                             , _mousePosition :: Maybe (Point 2 r)
                                             }

data CanvasAction = HandleMouse (Int, Int)
                  | Id
                  | DoStuff String
               -- | HandleTouch TouchEvent


updateCanvas                          :: Num r => CanvasAction -> Canvas r
                                      -> Effect CanvasAction (Canvas r)
updateCanvas (HandleMouse mp) cv = noEff $ cv&center .~ realWorldCoordinates cv mp
    -- noEff . Model $ cv&scale .~ (1 + (fromIntegral my / 576.0))
    -- TODO: only change the center when we are panning
updateCanvas Id                    cv = noEff cv
updateCanvas (DoStuff s)           cv = noEff cv -- <# do putStrLn s ; pure Id

-- updateModel (HandleTouch (TouchEvent touch)) model =
--   model <# do
--     -- putStrLn "Touch did move"
--     -- print touch
--     return $ HandleMouse $ trunc . page $ touch



-- data InteractiveCanvas r = InteractiveCanvas { _canvas :: Canvas r
--                                              , _mousePosition :: Maybe (Point 2 r)
--                                              }




viewModel            :: Model -> View Action
viewModel cv = div_ [ ] [
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
                              , text'_ @Int (100,20) [] [text "Woei!"]
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
