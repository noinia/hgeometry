{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Data.Geometry.Interactive.ICanvas( module Data.Geometry.Interactive.StaticCanvas
                                        , ICanvas(ICanvas), blankCanvas
                                        , canvas, mousePosition, mouseCoordinates

                                        , CanvasAction(..)
                                        , update
                                        , view
                                        ) where

import           Control.Lens hiding (view, element)
import           Data.Ext
import           Data.Geometry.Interactive.StaticCanvas
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Miso hiding (update, view)
import           Miso.String (MisoString, ToMisoString, ms)
import qualified Miso.Svg as Svg


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


data CanvasAction = GetMouseState (Int,Int)
                  | GetArrowsState Arrows
                  | Scroll
                  deriving (Show,Eq)


update   :: Num r => ICanvas r -> CanvasAction -> Effect action (ICanvas r)
update m = \case
    GetMouseState (x,y)         -> let p  = Point2 x y -- fix this part
                                   in noEff $ m&mousePosition .~ Just p
    GetArrowsState (Arrows x y) -> let v   = ((*2) . fromIntegral) <$> Vector2 x y
                                   in noEff $ m&canvas.center %~ (.+^ v)




-- * View

view   :: (RealFrac r, ToSvgCoordinate r)
       => ICanvas r -> [Attribute action] -> [View action] -> View action
view m = staticCanvas_ (m^.canvas)



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
