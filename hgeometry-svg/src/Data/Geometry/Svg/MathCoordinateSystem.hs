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
module Data.Geometry.Svg.MathCoordinateSystem( Canvas(Canvas)
                                             , center, dimensions, zoomLevel
                                             , createCanvas
                                             , renderCanvas, text_
                                             , realWorldCoordinates

                                             , toAValue, toPValue, showP
                                             ) where

import           Control.Lens hiding (view, element)
import           Data.Fixed
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.Text (Text)
import           Data.Util (SP(..))
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
    Vector2 w h = cv^.dimensions

    SP (Point2 lx ly) (Vector2 vw vh) = bimap (fmap round) (fmap round) $ viewRectangle cv


    toVB = mconcat @Svg.AttributeValue . List.intersperse " " . map toPValue
    outerVB = toVB [0, (-1) * h, w, h]
            -- the role of the outer viewBox is to flip the coordinate
            -- system s.t. the origin is in the bottom left rather
            -- than the top-left
    innerVB = toVB [lx, ly, vw, vh]

-- | Computes the view rectangle of the canvas; given by the lower left point and
-- the dimensions (in real coordinates).
viewRectangle    :: Fractional r => Canvas r -> SP (Point 2 r) (Vector 2 r)
viewRectangle cv = SP (Point2 (cx - (vw / 2)) (cy - (vh / 2)))
                      dims
  where
    Point2 cx cy         = cv^.center
    dims@(Vector2 vw vh) = (1 / cv^.zoomLevel) *^ (fromIntegral <$> cv^.dimensions)

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

-- | Computes the mouse position in terms of real world coordinates.
-- pre: the coordinates given lie on the canvas
realWorldCoordinates                 :: Fractional r => Canvas r -> Point 2 Int -> Point 2 r
realWorldCoordinates cv (Point2 x y) =
    applyViewBox cv $ Point2 x ((cv^.dimensions.element (C @ 1)) - y)
                      -- position relative to the outer viewbox

-- | Applies the viewbox transformation
applyViewBox      :: Fractional r => Canvas r -> Point 2 Int -> Point 2 r
applyViewBox cv p = Point2 (lx + (x/w) * vw) (ly + (y/h)*vh)
  where
    (Vector2 w h) = fromIntegral <$> cv^.dimensions
    SP (Point2 lx ly) (Vector2 vw vh) = viewRectangle cv
    Point2 x y = fromIntegral <$> p

--------------------------------------------------------------------------------

toAValue :: Show a => a -> Svg.AttributeValue
toAValue = Svg.toValue . show

toPValue :: Real r => r -> Svg.AttributeValue
toPValue = toAValue . showP

-- | show by converting to a Pico
showP :: Real a => a -> Pico
showP = realToFrac
