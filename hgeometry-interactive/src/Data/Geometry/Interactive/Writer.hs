{-# LANGUAGE OverloadedStrings          #-}
module Data.Geometry.Interactive.Writer where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.Ext
import           Data.Fixed
import qualified Data.Foldable as F
import           Data.Geometry.Interactive.StaticCanvas
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.Geometry.Vector
import qualified Data.List as List
import qualified Data.Map as Map
import           Miso
import           Miso.String (MisoString, ToMisoString(..), ms)
import qualified Miso.String.Util as MisoString
import           Miso.Svg

--------------------------------------------------------------------------------

type InteractiveOut g action = g -> [Attribute action] -> View action

-- (!) :: View action -> [Attribute action] -> View action

iSO'' :: g -> [Attribute action] -> View action
iSO'' = undefined



withAts             ::  ([Attribute action] -> [View action] -> View action)
                    -> [Attribute action] -> [Attribute action] -> View action
withAts f ats1 ats2 = withAts' f ats1 ats2 []

withAts'                  :: ([Attribute action] -> [View action] -> View action)
                          -> [Attribute action]
                          -> [Attribute action]
                          -> [View action]
                          -> View action
withAts' f ats1 ats2 body = f (ats1 <> ats2) body


instance HasResolution p => ToMisoString (Fixed p) where
  toMisoString = toMisoString . showFixed True
  fromMisoString = read . fromMisoString

instance ToMisoString Rational where
  toMisoString = toMisoString @Pico . realToFrac
  fromMisoString = realToFrac . fromMisoString @Pico

class Drawable t where
  draw       :: t -> [Attribute action] -> View action
  draw x ats = drawWith x ats []

  drawWith          :: t -> [Attribute action] -> [View action] -> View action
  drawWith x ats _b = draw x ats


instance ToMisoString r => Drawable (Point 2 r) where
  draw = cPoint

instance ToMisoString r => Drawable (Polygon t p r) where
  draw = cPolygon

instance ToMisoString r => Drawable (ConvexPolygon p r) where
  draw = cPolygon . (^.simplePolygon)


cPoint              :: ToMisoString r => Point 2 r -> [Attribute action] -> View action
cPoint (Point2 x y) = withAts ellipse_ [ cx_ $ ms x, cy_ $ ms y
                                       , rx_ "5", ry_ "5"
                                       ]


cPolygon                    :: ToMisoString r
                            => Polygon t p r -> [Attribute action] -> View action
cPolygon (SimplePolygon vs) = withAts polygon_ [points_ $ f vs ]
  where
    f = MisoString.unwords . map (\(Point2 x y :+ _) -> mconcat [ms x, ",", ms y]) . F.toList
