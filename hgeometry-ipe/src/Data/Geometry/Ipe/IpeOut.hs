{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Ipe.IpeOut
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
module Data.Geometry.Ipe.IpeOut where


import           Control.Lens hiding (Simple)
import           Data.Bifunctor
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Ball
import           Data.Geometry.Ellipse(Ellipse, circleToEllipse)
import           Data.Geometry.BezierSpline
import           Data.Geometry.Boundary
import           Data.Geometry.Box
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Color (IpeColor(..))
import           Data.Geometry.Ipe.FromIpe
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine (PolyLine,fromLineSegment)
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.Geometry.Properties
import qualified Data.LSeq as LSeq
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vinyl.CoRec
import           Linear.Affine ((.+^))

--------------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :{
-- let myPolygon = fromPoints . map ext $ [origin, Point2 10 10, Point2 100 200]
-- :}

--------------------------------------------------------------------------------
-- * The IpeOut type and the default combinator to use it

type IpeOut g i r = g -> IpeObject' i r


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
-- IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "blue"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> :{
--   iO $ ipeGroup [ iO $ ipePolygon myPolygon ! attr SFill (IpeColor "red")
--                 ] ! attr SLayer "alpha"
-- :}
-- IpeGroup (Group [IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
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
-- IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> iO'' [ myPolygon , myPolygon ] $ attr SLayer "alpha"
-- IpeGroup (Group [IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr}),IpePath (Path {_pathSegments = LSeq (fromList [PolygonPath SimplePolygon CSeq [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [100,200] :+ ()]])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
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
  type DefaultIpeOut g :: * -> *

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

instance HasDefaultIpeOut (LineSegment 2 p r) where
  type DefaultIpeOut (LineSegment 2 p r) = Path
  defIO = ipeLineSegment

instance HasDefaultIpeOut (PolyLine 2 p r) where
  type DefaultIpeOut (PolyLine 2 p r) = Path
  defIO = ipePolyLine

instance (Fractional r, Ord r) => HasDefaultIpeOut (Line 2 r) where
  type DefaultIpeOut (Line 2 r) = Path
  defIO = ipeLineSegment . toSeg
    where
      b :: Rectangle () r
      b = box (ext $ Point2 (-200) (-200)) (ext $ Point2 1200 1200)
      naive (Line p v) = ClosedLineSegment (ext p) (ext $ p .+^ v)
      toSeg l = fromMaybe (naive l) . asA @(LineSegment 2 () r)
              $ l `intersect` b

instance HasDefaultIpeOut (Polygon t p r) where
  type DefaultIpeOut (Polygon t p r) = Path
  defIO = ipePolygon

instance HasDefaultIpeOut (SomePolygon p r) where
  type DefaultIpeOut (SomePolygon p r) = Path
  defIO = either defIO defIO

instance HasDefaultIpeOut (ConvexPolygon p r) where
  type DefaultIpeOut (ConvexPolygon p r) = Path
  defIO = defIO . view simplePolygon

instance HasDefaultIpeOut (Ellipse r) where
  type DefaultIpeOut (Ellipse r) = Path
  defIO = ipeEllipse

instance Floating r => HasDefaultIpeOut (Disk p r) where
  type DefaultIpeOut (Disk p r) = Path
  defIO = ipeDisk

instance Floating r => HasDefaultIpeOut (Circle p r) where
  type DefaultIpeOut (Circle p r) = Path
  defIO = ipeCircle

instance Num r => HasDefaultIpeOut (Rectangle p r) where
  type DefaultIpeOut (Rectangle p r) = Path
  defIO = ipeRectangle

--------------------------------------------------------------------------------
-- * Point Converters

ipeMark     :: Text -> IpeOut (Point 2 r) IpeSymbol r
ipeMark n p = Symbol p n :+ mempty

ipeDiskMark :: IpeOut (Point 2 r) IpeSymbol r
ipeDiskMark = ipeMark "mark/disk(sx)"

--------------------------------------------------------------------------------
-- * Path Converters

ipeLineSegment   :: IpeOut (LineSegment 2 p r) Path r
ipeLineSegment s = (path . pathSegment $ s) :+ mempty

ipePolyLine   :: IpeOut (PolyLine 2 p r) Path r
ipePolyLine p = (path . PolyLineSegment . first (const ()) $ p) :+ mempty

ipeEllipse :: IpeOut (Ellipse r) Path r
ipeEllipse = \e -> (path $ EllipseSegment e) :+ mempty

ipeCircle :: Floating r => IpeOut (Circle p r) Path r
ipeCircle = ipeEllipse . circleToEllipse

ipeDisk   :: Floating r => IpeOut (Disk p r) Path r
ipeDisk d = ipeCircle (Boundary d) ! attr SFill (IpeColor "0.722 0.145 0.137")

-- | Helper to construct a path from a singleton item
path :: PathSegment r -> Path r
path = Path . LSeq.fromNonEmpty . (:| [])

pathSegment :: LineSegment 2 p r -> PathSegment r
pathSegment = PolyLineSegment . fromLineSegment . first (const ())

-- | Draw a polygon
ipePolygon                          :: IpeOut (Polygon t p r) Path r
ipePolygon (first (const ()) -> pg) = case pg of
               (SimplePolygon _)  -> pg^.re _asSimplePolygon :+ mempty
               (MultiPolygon _ _) -> pg^.re _asMultiPolygon  :+ mempty


-- | Draw a Rectangle
ipeRectangle   :: Num r => IpeOut (Rectangle p r) Path r
ipeRectangle r = ipePolygon $ fromPoints [tl,tr,br,bl]
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
labelled                 :: (Show lbl, NumType g ~ r, ToObject i)
                         => (g -> Point 2 r) -- ^ where to place the label
                         -> IpeOut g i r     -- ^ how to draw the geometric object
                         -> IpeOut (g :+ lbl) Group r
labelled pos f (g :+ lbl) = ipeGroup [iO $ f g, iO $ ipeLabel ((Text.pack $ show lbl) :+ pos g)]
