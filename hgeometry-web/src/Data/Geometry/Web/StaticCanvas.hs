{-# LANGUAGE OverloadedStrings          #-}
module Data.Geometry.Web.StaticCanvas(
    Canvas(..)
    , center, dimensions, zoomLevel, createCanvas
    , staticCanvas_, textAt
    , realWorldCoordinates
    , ToSvgCoordinate
    ) where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.Geometry.Point
import           Data.Geometry.Svg.MathCoordinateSystem ( Canvas(..)
                                                        , center
                                                        , dimensions
                                                        , zoomLevel
                                                        , createCanvas
                                                        , realWorldCoordinates
                                                        )
import           Data.Geometry.Vector
import qualified Data.List as List
import qualified Data.Map as Map
import           Miso
import qualified Miso.Svg as Miso
import           Miso.String (MisoString, ToMisoString, ms)
import qualified Miso.String.Util as MisoString
import           Miso.Svg(svg_, g_, transform_, viewBox_)

--------------------------------------------------------------------------------

type ToSvgCoordinate = ToMisoString









-- | Draws the actual canvas
staticCanvas_           :: (RealFrac r, ToSvgCoordinate r)
                       =>  Canvas r -> [Attribute action] -> [View action] -> View action
staticCanvas_ cv ats vs = div_ [ style_ $ Map.fromList [ ("margin", "0")
                                                       , ("padding", "0")
                                                       , ("height",  ms h <> "px")
                                                       ]
                               ]
                               [ svg_ ([ width_   . ms $ w
                                       , height_  . ms $ h
                                       , viewBox_      $ outerVB
                                       ] <> ats
                                      )
                                      [ g_ [ transform_ "scale(1,-1)" ]
                                           [ svg_ [ width_  . ms $ w
                                                  , height_ . ms $ h
                                                  , viewBox_ $ innerVB
                                                  ]  vs
                                           ]
                                      ]
                               ]

  where
    dims@(Vector2 w h) = cv^.dimensions
    Point2 cx cy       = round <$> cv^.center

    Vector2 vw vh = round  <$> (1 / cv^.zoomLevel) *^ (fromIntegral <$> dims)


    toVB = MisoString.unwords . map ms
    outerVB = toVB [0, (-1) * h, w, h]
      -- role of the outer viewBox is to flip the coordinate system s.t. the origin
      -- is in the bottom left rather than the top-left

    innerVB = toVB [(cx - (vw `div` 2)), (cy - (vh `div` 2)), vw, vh]


-- | To be used instead of the text_ combinator in Miso
textAt                     :: ToSvgCoordinate r
                           => Point 2 r -- ^ position where to draw (in world coordinates)
                           -> [Attribute action]
                           -> MisoString -> View action
textAt (Point2 x y) ats t = g_ [ transform_ $ mconcat [ "translate("
                                                       , ms x
                                                       , ", "
                                                       , ms y
                                                       , ")scale(1,-1)"
                                                       ]
                                ] [ Miso.text_ ats [text t] ]



-- class RenderWebSvg t where
--   renderWith :: t -> [Attribute action] -> [View action] -> View action

-- render       :: RenderWebSvg t => t -> [Attribute action] -> View action
-- render x ats = renderWith x ats []

-- instance ToSvgCoordinate r => RenderWebSvg (Point 2 r) where
--   renderWith (Point2 x y) ats = ellipse_ $ [ cx_ $ ms x, cy_ $ ms y
--                                            , rx_ "5", ry_ "5"
--                                            , fill_ "black"
--                                            ] <> ats
