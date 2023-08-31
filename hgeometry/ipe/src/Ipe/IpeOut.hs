{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.IpeOut
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functions that help drawing geometric values in ipe. An "IpeOut" is
-- essenitally a function that converts a geometric type g into an IpeObject.
--
-- We also proivde a "HasDefaultIpeOut" typeclass that defines a default
-- conversion function from a geometry type g to an ipe type.
--
--------------------------------------------------------------------------------
module Ipe.IpeOut where

import           Control.Lens hiding (Simple)
import           Data.Foldable (toList)
import           Data.Kind
import qualified Data.Sequence as Seq
-- import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as Text
-- import           Data.Vinyl (Rec(..))
-- import           Data.Vinyl.CoRec
import           HGeometry.Ball
import           HGeometry.BezierSpline
import           HGeometry.Box
import           HGeometry.Ellipse (Ellipse, circleToEllipse)
import           HGeometry.Ext
-- import           HGeometry.HalfLine
import           HGeometry.Line
-- import           HGeometry.LineSegment
import           HGeometry.Number.Radical
import           HGeometry.Point
import           HGeometry.Intersection
import           HGeometry.PolyLine
import           HGeometry.LineSegment
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           Ipe.Attributes
import           Ipe.Color (IpeColor(..))
-- import           Ipe.Content (IpeAttributes)
import           Ipe.FromIpe
import           Ipe.Types


--------------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :{
-- let Just (myPolygon :: SimplePolygon (Point 2 Int)) = fromPoints $ [origin, Point2 10 10, Point2 100 200]
-- :}

--------------------------------------------------------------------------------
-- * The IpeOut type and the default combinator to use it

type IpeOut g i r = g -> IpeObject' i r

-- | Give the option to draw zero, one or more things, i.e. by
-- choosing f ~ Maybe or f ~ []
type IpeOut' f g i r = g -> f (IpeObject' i r)


-- | Add attributes to an IpeObject'
(!)       :: IpeObject' i r -> IpeAttributes i r -> IpeObject' i r
(!) i ats = i&extra %~ (<> ats)

-- | Render an ipe object
--
--
-- >>> :{
--   iO $ defIO myPolygon ! attr SFill (IpeColor "blue")
--                        ! attr SLayer "alpha"
--                        ! attr SLayer "beta"
-- :}
-- IpePath (Path {_pathSegments = fromList [PolygonPath SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200]]} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "blue"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> :{
--   iO $ ipeGroup [ iO $ ipePolygon myPolygon ! attr SFill (IpeColor "red")
--                 ] ! attr SLayer "alpha"
-- :}
-- IpeGroup (Group [IpePath (Path {_pathSegments = fromList [PolygonPath SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200]]} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
--
iO :: ToObject i => IpeObject' i r -> IpeObject r
iO = mkIpeObject

-- | Render to an ipe object using the defIO IpeOut
--
--
-- >>> :{
--   iO'' myPolygon $  attr SFill (IpeColor "red")
--                  <> attr SLayer "alpha"
--                  <> attr SLayer "beta"
-- :}
-- IpePath (Path {_pathSegments = fromList [PolygonPath SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200]]} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> iO'' [ myPolygon , myPolygon ] $ attr SLayer "alpha"
-- IpeGroup (Group [IpePath (Path {_pathSegments = fromList [PolygonPath SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200]]} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr}),IpePath (Path {_pathSegments = fromList [PolygonPath SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200]]} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
iO''       :: ( HasDefaultIpeOut g, NumType g ~ r
             , DefaultIpeOut g ~ i, ToObject i
             ) => g -> IpeAttributes i r
           -> IpeObject r
iO'' g ats = iO $ defIO g ! ats

-- | generate an ipe object without any specific attributes
iO' :: HasDefaultIpeOut g => g -> IpeObject (NumType g)
iO' = iO . defIO

--------------------------------------------------------------------------------
-- * Default Conversions

-- | Class that specifies a default conversion from a geometry type g into an
-- ipe object.
class ToObject (DefaultIpeOut g) => HasDefaultIpeOut g where
  type DefaultIpeOut g :: Type -> Type

  defIO :: IpeOut g (DefaultIpeOut g) (NumType g)

instance (HasDefaultIpeOut g, a ~ IpeAttributes (DefaultIpeOut g) (NumType g))
        => HasDefaultIpeOut (g :+ a) where
  type DefaultIpeOut (g :+ a) = DefaultIpeOut g
  defIO (g :+ ats) = defIO g ! ats

instance HasDefaultIpeOut a => HasDefaultIpeOut [a] where
  type DefaultIpeOut [a] = Group
  defIO = ipeGroup . map (iO .  defIO)

instance HasDefaultIpeOut (Point 2 r) where
  type DefaultIpeOut (Point 2 r) = IpeSymbol
  defIO = ipeDiskMark

instance ( IxValue (endPoint (Point 2 r)) ~ Point 2 r
         , EndPoint_ (endPoint (Point 2 r))
         ) => HasDefaultIpeOut (LineSegment endPoint (Point 2 r)) where
  type DefaultIpeOut (LineSegment endPoint (Point 2 r)) = Path
  defIO = ipeLineSegment

instance HasDefaultIpeOut (PolyLine (Point 2 r)) where
  type DefaultIpeOut (PolyLine (Point 2 r)) = Path
  defIO = ipePolyLine

instance (Fractional r, Ord r) => HasDefaultIpeOut (LinePV 2 r) where
  type DefaultIpeOut (LinePV 2 r) = Path
  defIO = ipeLine

-- instance (Fractional r, Ord r) => HasDefaultIpeOut (HalfLine 2 r) where
--   type DefaultIpeOut (HalfLine 2 r) = Path
--   defIO = ipeHalfLine

instance HasDefaultIpeOut (SimplePolygon (Point 2 r)) where
  type DefaultIpeOut (SimplePolygon (Point 2 r)) = Path
  defIO = ipePolygon

-- instance HasDefaultIpeOut (SomePolygon p r) where
--   type DefaultIpeOut (SomePolygon p r) = Path
--   defIO = either defIO defIO

instance HasDefaultIpeOut (ConvexPolygon (Point 2 r)) where
  type DefaultIpeOut (ConvexPolygon (Point 2 r)) = Path
  defIO = defIO . toSimplePolygon

instance HasDefaultIpeOut (Ellipse r) where
  type DefaultIpeOut (Ellipse r) = Path
  defIO = ipeEllipse

instance Radical r => HasDefaultIpeOut (Disk (Point 2 r)) where
  type DefaultIpeOut (Disk (Point 2 r)) = Path
  defIO = ipeDisk

instance Radical r => HasDefaultIpeOut (Circle (Point 2 r)) where
  type DefaultIpeOut (Circle (Point 2 r)) = Path
  defIO = ipeCircle

instance Num r => HasDefaultIpeOut (Rectangle (Point 2 r)) where
  type DefaultIpeOut (Rectangle (Point 2 r)) = Path
  defIO = ipeRectangle

--------------------------------------------------------------------------------
-- * Point Converters

ipeMark     :: Text -> IpeOut (Point 2 r) IpeSymbol r
ipeMark n p = Symbol p n :+ mempty

ipeDiskMark :: IpeOut (Point 2 r) IpeSymbol r
ipeDiskMark = ipeMark "mark/disk(sx)"

--------------------------------------------------------------------------------
-- * Path Converters

-- | Size of the default bounding box used to clip lines and
-- half-lines in the default IpeOuts.
defaultBox :: Num r => Rectangle (Point 2 r)
defaultBox = let z  = 1000
                 z' = negate z
             in Rectangle (Point2 z' z') (Point2 z z)

-- | Renders a line as a Path. The line is clipped to the 'defaultBox'
ipeLine :: (Ord r, Fractional r) => IpeOut (LinePV 2 r) Path r
ipeLine = ipeLineIn defaultBox

-- | Renders the line in the given box.
--
-- pre: the intersection of the box with the line is non-empty
ipeLineIn        :: forall r. (Ord r, Fractional r)
                 => Rectangle (Point 2 r) -> IpeOut (LinePV 2 r) Path r
ipeLineIn bBox l = case l `intersect` bBox of
  Nothing                     -> error "ipeLineIn: precondition failed, no intersection"
  Just (Line_x_Box_Point _)   -> error "ipeLineIn: precondition failed, single point"
  Just (Line_x_Box_Segment s) -> ipeLineSegment s

-- -- | Renders an Halfine.
-- --
-- --
-- -- pre: the intersection of the box with the line is non-empty
-- ipeHalfLine :: (Ord r, Fractional r) => IpeOut (HalfLine 2 r) Path r
-- ipeHalfLine = ipeHalfLineIn defaultBox

-- -- | Renders a ray, i.e. a half line drawing an arrow in the direction
-- -- of the ray.
-- --
-- -- pre: the intersection of the box with the line is non-empty
-- ipeRay :: (Ord r, Fractional r) => IpeOut (HalfLine 2 r) Path r
-- ipeRay = \hl -> ipeHalfLine hl ! attr SArrow normalArrow


-- -- | Renders the HalfLine in the given box.
-- --
-- -- pre: the intersection of the box with the line is non-empty
-- ipeHalfLineIn        :: forall p r. (Ord r, Fractional r)
--                      => Rectangle p r -> IpeOut (HalfLine 2 r) Path r
-- ipeHalfLineIn bBox l = match (l `intersect` bBox) $
--      H (\NoIntersection    -> error "ipeHalfLineIn: precondition failed, no intersection")
--   :& H (\(_p :: Point 2 r) -> error "ipeHalfLineIn: precondition failed, single point")
--   :& H ipeLineSegment
--   :& RNil

ipeLineSegment   :: (LineSegment_ lineSegment point, Point_ point 2 r)
                 => IpeOut lineSegment Path r
ipeLineSegment s = (path . pathSegment $ s) :+ mempty

ipePolyLine   :: IpeOut (PolyLine (Point 2 r)) Path r
ipePolyLine p = (path . PolyLineSegment $ p) :+ mempty

ipeEllipse :: IpeOut (Ellipse r) Path r
ipeEllipse = \e -> path (EllipseSegment e) :+ mempty

ipeCircle :: Radical r => IpeOut (Circle (Point 2 r)) Path r
ipeCircle = ipeEllipse . circleToEllipse

ipeDisk   :: Radical r => IpeOut (Disk (Point 2 r)) Path r
ipeDisk d = ipeCircle (MkSphere d) ! attr SFill (IpeColor "0.722 0.145 0.137")

ipeBezier :: IpeOut (CubicBezier (Point 2 r)) Path r
ipeBezier b = (path $ CubicBezierSegment b) :+ mempty

-- | Helper to construct a path from a singleton item
path :: PathSegment r -> Path r
path = Path . Seq.singleton

pathSegment :: (LineSegment_ lineSegment point, Point_ point 2 r)
            => lineSegment -> PathSegment r
pathSegment = PolyLineSegment . fmap (^.asPoint) . review _PolyLineLineSegment

-- | Draw a polygon
ipePolygon    :: IpeOut (SimplePolygon (Point 2 r)) Path r
ipePolygon pg = pg^.re _asSimplePolygon :+ mempty
-- (first (const ()) -> pg) = case pg of
--                SimplePolygon{} -> pg^.re _asSimplePolygon :+ mempty
--                -- MultiPolygon{}  -> pg^.re _asMultiPolygon  :+ mempty


-- | Draw a Rectangle
ipeRectangle   :: Num r => IpeOut (Rectangle (Point 2 r)) Path r
ipeRectangle r = ipePolygon $ uncheckedFromCCWPoints [tl,tr,br,bl]
  where
    Corners tl tr br bl = corners r

--------------------------------------------------------------------------------
-- * Group Converters

ipeGroup    :: Foldable f => IpeOut (f (IpeObject r)) Group r
ipeGroup xs = Group (toList xs) :+ mempty


--------------------------------------------------------------------------------
-- * Text Converters

-- | Creates an text label
ipeLabel            :: IpeOut (Text :+ Point 2 r) TextLabel r
ipeLabel (txt :+ p) = Label txt p :+ mempty


-- | Annotate an IpeOut with a label
labelled :: (Show lbl, NumType g ~ r, ToObject i)
         => (g -> Point 2 r) -- ^ where to place the label
         -> IpeOut g i r     -- ^ how to draw the geometric object
         -> IpeOut (g :+ lbl) Group r
labelled = labelledWith mempty

-- | Annotate an IpeOut with a label
labelledWith                      :: (Show lbl, NumType g ~ r, ToObject i)
                                  => IpeAttributes TextLabel r -- ^ attributes for the label
                                  -> (g -> Point 2 r) -- ^ where to place the label
                                  -> IpeOut g i r     -- ^ how to draw the geometric object
                                  -> IpeOut (g :+ lbl) Group r
labelledWith ats pos f (g :+ lbl) = ipeGroup [ iO $ f g
                                     , iO $ ipeLabel (Text.pack (show lbl) :+ pos g) ! ats
                                     ]
