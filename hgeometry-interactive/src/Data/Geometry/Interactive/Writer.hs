{-# LANGUAGE OverloadedStrings          #-}
module Data.Geometry.Interactive.Writer where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.Ext
import           Data.Fixed
import qualified Data.Foldable as F
import           Data.Geometry.Interactive.StaticCanvas
import           Data.Geometry.Point
import           Data.Geometry.Ball
import           Data.Geometry.PolyLine
import           Data.Geometry.LineSegment
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


-- | Helper function to construct drawing functions. I..e it allows
-- you do pre-specify a bunch of attributes that should be drawn
-- (ats1) yet allow more attributes to be added by the user later.
withAts             ::  ([Attribute action] -> [View action] -> View action)
                    -> [Attribute action] -> [Attribute action] -> View action
withAts f ats1 ats2 = withAts' f ats1 ats2 []

-- | Helper function to construct a View. See 'withAts' for its usage.
withAts'                  :: ([Attribute action] -> [View action] -> View action)
                          -> [Attribute action]
                          -> [Attribute action]
                          -> [View action]
                          -> View action
withAts' f ats1 ats2 body = f (ats1 <> ats2) body


--------------------------------------------------------------------------------

instance HasResolution p => ToMisoString (Fixed p) where
  toMisoString = toMisoString . showFixed True
  fromMisoString = read . fromMisoString

instance ToMisoString Rational where
  toMisoString = toMisoString @Pico . realToFrac
  fromMisoString = realToFrac . fromMisoString @Pico


--------------------------------------------------------------------------------
-- * Default implementations for drawing geometric objects

-- | Default implementation for drawing geometric objects
class Drawable t where
  draw       :: t -> [Attribute action] -> View action
  draw x ats = drawWith x ats []

  drawWith          :: t -> [Attribute action] -> [View action] -> View action
  drawWith x ats _b = draw x ats


instance ToMisoString r => Drawable (Point 2 r) where
  draw = cPoint

instance ToMisoString r => Drawable (LineSegment 2 p r) where
  draw = cLineSegment

instance ToMisoString r => Drawable (PolyLine 2 p r) where
  draw = cPolyLine

instance ToMisoString r => Drawable (Polygon t p r) where
  draw = cPolygon

instance ToMisoString r => Drawable (ConvexPolygon p r) where
  draw = cPolygon . (^.simplePolygon)


instance (ToMisoString r, Floating r) => Drawable (Circle p r) where
  draw = cCircle

instance (ToMisoString r, Floating r) => Drawable (Disk p r) where
  draw = cDisk


--------------------------------------------------------------------------------
-- * Functions to draw geometric objects


cPoint              :: ToMisoString r => Point 2 r -> [Attribute action] -> View action
cPoint (Point2 x y) = withAts ellipse_ [ cx_ $ ms x, cy_ $ ms y
                                       , rx_ "5", ry_ "5"
                                       ]


cPolygon                      :: ToMisoString r
                              => Polygon t p r -> [Attribute action] -> View action
cPolygon (SimplePolygon vs)   = withAts polygon_ [points_ $ toPointsString vs ]
cPolygon (MultiPolygon vs hs) = withAts polygon_ [points_ $ toPointsString vs ]
                                -- FIXME: this does not draw the holes

cPolyLine    :: ToMisoString r => PolyLine 2 p r -> [Attribute action] -> View action
cPolyLine pl = withAts polyline_ [points_ . toPointsString $ pl^.points ]

cLineSegment   :: ToMisoString r => LineSegment 2 p r -> [Attribute action] -> View action
cLineSegment s = cPolyLine (fromLineSegment s)


-- | constructs a list of points to be used in the 'points' svg attribute.
toPointsString :: (ToMisoString r, Foldable f) => f (Point 2 r :+ p) -> MisoString
toPointsString =
  MisoString.unwords . map (\(Point2 x y :+ _) -> mconcat [ms x, ",", ms y]) . F.toList



cCircle              :: (ToMisoString r, Floating r)
                     => Circle p r -> [Attribute action] -> View action
cCircle (Circle c r) = withAts ellipse_ [ rx_ . ms $ r
                                        , ry_ . ms $ r
                                        , cx_ . ms $ c^.core.xCoord
                                        , cy_ . ms $ c^.core.yCoord
                                        ]

cDisk            :: (ToMisoString r, Floating r)
                 => Disk p r -> [Attribute action] -> View action
cDisk (Disk c r) = cCircle (Circle c r)
