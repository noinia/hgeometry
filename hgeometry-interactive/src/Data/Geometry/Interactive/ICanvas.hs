{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
module Data.Geometry.Interactive.ICanvas( module Data.Geometry.Interactive.StaticCanvas
                                        , ICanvas(ICanvas), blankCanvas
                                        , canvas, mousePosition
                                        , mouseCoordinates

                                        , CanvasAction(..)
                                        , update
                                        , view
                                        ) where

import           Control.Lens hiding (view, element)
import           Data.Geometry.Interactive.StaticCanvas
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Aeson.Types
import           Miso hiding (update, view)
import qualified Data.Map as Map

import Debug.Trace
--------------------------------------------------------------------------------

-- * Model

data ICanvas r = ICanvas { _canvas        :: Canvas r
                         , _mousePosition :: Maybe (Point 2 Int)
                         } deriving (Show,Eq)
makeLenses ''ICanvas


mouseCoordinates :: Num r => Getter (ICanvas r) (Maybe (Point 2 r))
mouseCoordinates = to $ \m -> realWorldCoordinates (m^.canvas) <$> m^.mousePosition


-- | Createas an interactive lbank canvas
blankCanvas     :: Num r => Int -> Int -> ICanvas r
blankCanvas w h = ICanvas (createCanvas w h) Nothing



-- * Controller


data CanvasAction = MouseMove (Int,Int)
                  | MouseLeave
                  | ArrowPress Arrows
                  deriving (Show,Eq)


update   :: Num r => ICanvas r -> CanvasAction -> Effect action (ICanvas r)
update m = \case
    MouseMove (x,y)         -> let p  = Point2 x y
                               in noEff $ m&mousePosition .~ Just p
    MouseLeave              -> noEff $ m&mousePosition .~ Nothing
    ArrowPress (Arrows x y) -> let v   = ((*2) . fromIntegral) <$> Vector2 x y
                               in noEff $ m&canvas.center %~ (.+^ v)


-- * View

view            :: (RealFrac r, ToSvgCoordinate r)
                => (CanvasAction -> action)
                -> ICanvas r
                -> [Attribute action] -> [View action] -> View action
view f m ats vs = staticCanvas_ (m^.canvas) ([ onMouseMove  (f . MouseMove)
                                             , onMouseLeave (f MouseLeave)
                                             , style_ (Map.fromList [("margin-left", "100px")])
                                             ] <> ats) vs


onMouseMove   :: ((Int,Int) -> action) -> Attribute action
onMouseMove f = on "mousemove" dec f
  where
    dec  :: Decoder (Int,Int)
    dec  = Decoder decF (DecodeTarget mempty)
    decF = withObject "event" $ \o -> g <$> o .: "clientX" <*> o .: "clientY"
      where
        g x y = traceShowId (x,y)







    -- decodeAt =
    -- decoder =
    --    KeyCode <$> (o .: "keyCode" <|> o .: "which" <|> o .: "charCode")


-- view            :: ICanvas r -> View action
-- view cv = div_ [ ] [
--       renderCanvas cv
--                               [
--                               ]
--                               [ ellipse_ [ cx_ "512", cy_ "288", rx_ "50", ry_ "50"
--                                          , fill_ "red"
--                                          , onClick $ DoStuff "woei"
--                                          ] []
--                               -- , rect_ [width_"100000", height_ "2000" , x_ "-1000", y_ "-1000"
--                               --         , fill_ "blue"
--                               --         ] []
--                               , textAt_ @Int (100,20) [] [text "Woei!"]
--                               ]
--    ]
--     -- svg'_ [
--     --       ]
--     --       [ ellipse_ [ cx_ $ ms x
--     --                  , cy_ $ ms y
--     --                  , style_ svgStyle
--     --                  , rx_ "100"
--     --                  , ry_ "100"
--     --                  ] [ ]
--     --       , text'_ (x,y) [] [text $ ms $ show (x,y) ]
--     --       ]
