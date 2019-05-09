{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Svg.MathCoordinateSystem
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :
--
-- Defines functions to make sure we render the coordinate system in
-- svg correctly, i.e. with the origin in the bottom-left instead of
-- top-left.
--
--------------------------------------------------------------------------------
module Data.Geometry.Svg.MathCoordinateSystem where

import           Control.Lens hiding (view, element)
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.Text (Text)
import           Data.Fixed
import           Prelude hiding ((!!))
import           Text.Blaze.Internal (Attributable(..))
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as Svg
import qualified Text.Blaze.Svg11.Attributes as A

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

--------------------------------------------------------------------------------

-- | Create a canvas
createCanvas     :: Num r => Int -> Int -> Canvas r
createCanvas w h = Canvas (Vector2 w h) (fromIntegral <$> Point2 (w `div` 2) (h `div` 2)) 1

--------------------------------------------------------------------------------


-- | Draws the actual canvas
renderCanvas           :: RealFrac r
                       =>  Canvas r -> [Svg.Attribute] -> Svg.Svg -> Svg.Svg
renderCanvas cv ats vs = Svg.svg ! A.width   (toPValue w)
                                 ! A.height  (toPValue h)
                                 ! A.viewbox outerVB
                                 ! A.style   "border-style: solid"
                                 !! ats
                                 $ Svg.g ! A.transform "scale(1,-1)"
                                         $ Svg.svg ! A.width  "100%"
                                                   ! A.height "100%"
                                                   ! A.viewbox innerVB
                                                   $ vs
  where
    dims@(Vector2 w h) = cv^.dimensions
    Point2 cx cy       = round <$> cv^.center

    Vector2 vw vh = round  <$> (1 / cv^.zoomLevel) *^ (fromIntegral <$> dims)

    toVB = mconcat @Svg.AttributeValue . List.intersperse " " . map toPValue
    outerVB = toVB [0, (-1) * h, w, h]
      -- role of the outer viewBox is to flip the coordinate system s.t. the origin
      -- is in the bottom left rather than the top-left

    innerVB = toVB [(cx - (vw `div` 2)), (cy - (vh `div` 2)), vw, vh]

infixl 9 !!
(!!) :: Attributable t => t -> [Svg.Attribute] -> t
t !! ats = List.foldl' (!) t ats


-- | To be used instead of the text_ combinator in Blaze
text_                    :: Real r
                         => Point 2 r -- ^ position where to draw (in world coordinates)
                         -> [Svg.Attribute]
                         -> Text -> Svg.Svg
text_ (Point2 x y) ats t = Svg.g ! A.transform (mconcat [ "translate("
                                                , toPValue x
                                                , ", "
                                                , toPValue y
                                                , ")scale(1,-1)"
                                                ])
                              $ Svg.text_ !! ats
                                          $ Svg.text t

--------------------------------------------------------------------------------

toAValue :: Show a => a -> Svg.AttributeValue
toAValue = Svg.toValue . show

toPValue :: Real r => r -> Svg.AttributeValue
toPValue = toAValue . showP

-- | show by converting to a Pico
showP :: Real a => a -> Pico
showP = realToFrac
