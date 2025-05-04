{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
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

import           Control.Lens hiding (Simple, holes)
import           Data.Foldable (toList)
import           Data.Foldable1 (Foldable1)
import           Data.Kind
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector.NonEmpty (NonEmptyVector)
import           HGeometry.Ball
import           HGeometry.Disk
import           HGeometry.BezierSpline
import           HGeometry.Box
import           HGeometry.Ellipse (Ellipse, circleToEllipse)
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.LineSegment
import           HGeometry.Number.Radical
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Polygon.WithHoles
import           HGeometry.Properties
import           HGeometry.Triangle (Triangle,toCounterClockwiseTriangle)
import           Ipe.Attributes
import           Ipe.Color (IpeColor(..), gray)
import           Ipe.FromIpe
import           Ipe.Path (Orientation(..))
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
-- IpePath (Path {_pathSegments = fromList [PolygonPath AsIs (SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200])]} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "blue"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> :{
--   iO $ ipeGroup [ iO $ ipePolygon myPolygon ! attr SFill (IpeColor "red")
--                 ] ! attr SLayer "alpha"
-- :}
-- IpeGroup (Group [IpePath (Path {_pathSegments = fromList [PolygonPath AsIs (SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200])]} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
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
-- IpePath (Path {_pathSegments = fromList [PolygonPath AsIs (SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200])]} :+ Attrs {Attr LayerName {_layerName = "beta"}, NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
--
-- >>> iO'' [ myPolygon , myPolygon ] $ attr SLayer "alpha"
-- IpeGroup (Group [IpePath (Path {_pathSegments = fromList [PolygonPath AsIs (SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200])]} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr}),IpePath (Path {_pathSegments = fromList [PolygonPath AsIs (SimplePolygon [Point2 0 0,Point2 10 10,Point2 100 200])]} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})] :+ Attrs {Attr LayerName {_layerName = "alpha"}, NoAttr, NoAttr, NoAttr, NoAttr})
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

instance HasDefaultIpeOut a => HasDefaultIpeOut (NonEmpty a) where
  type DefaultIpeOut (NonEmpty a) = Group
  defIO = ipeGroup . map (iO .  defIO) . toList

instance (HasDefaultIpeOut a, HasDefaultIpeOut b
         , DefaultIpeOut a ~ DefaultIpeOut b, NumType a ~ NumType b
         ) => HasDefaultIpeOut (Either a b) where
  type DefaultIpeOut (Either a b) = DefaultIpeOut a
  defIO = either defIO defIO

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

instance (Eq r, Num r) => HasDefaultIpeOut (Triangle (Point 2 r)) where
  type DefaultIpeOut (Triangle (Point 2 r)) = Path
  defIO = ipeTriangle

instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (LinePV 2 r) where
  type DefaultIpeOut (LinePV 2 r) = Path
  defIO = ipeLine

instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (LineEQ r) where
  type DefaultIpeOut (LineEQ r) = Path
  defIO (LineEQ a b) = ipeLine $ fromLinearFunction a b

instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (VerticalOrLineEQ r) where
  type DefaultIpeOut (VerticalOrLineEQ r) = Path
  defIO = \case
    VerticalLineThrough x -> ipeLine $ verticalLine x
    NonVertical l         -> defIO l

instance (Fractional r, Ord r, Point_ point 2 r,Show r, Show point) => HasDefaultIpeOut (HalfLine point) where
  type DefaultIpeOut (HalfLine point) = Path
  defIO = ipeHalfLine

instance ( VertexContainer f (Point 2 r)
         , HasFromFoldable1 f
         ) => HasDefaultIpeOut (SimplePolygonF f (Point 2 r)) where
  type DefaultIpeOut (SimplePolygonF f (Point 2 r)) = Path
  defIO = ipePolygon

instance ( HoleContainer h f (Point 2 r)
         , HasFromFoldable1 f
         )
       => HasDefaultIpeOut (PolygonalDomainF h f (Point 2 r)) where
  type DefaultIpeOut (PolygonalDomainF h f (Point 2 r)) = Path
  defIO = ipePolygon

instance ( VertexContainer f (Point 2 r)
         , HasFromFoldable1 f
         ) => HasDefaultIpeOut (ConvexPolygonF f (Point 2 r)) where
  type DefaultIpeOut (ConvexPolygonF f (Point 2 r)) = Path
  defIO = defIO . toSimplePolygon

instance HasDefaultIpeOut (Ellipse r) where
  type DefaultIpeOut (Ellipse r) = Path
  defIO = ipeEllipse

instance Radical r => HasDefaultIpeOut (Disk (Point 2 r)) where
  type DefaultIpeOut (Disk (Point 2 r)) = Path
  defIO = ipeDisk

instance (Radical r, Fractional r, Point_ point 2 r
         ) => HasDefaultIpeOut (DiametralBall point) where
  type DefaultIpeOut (DiametralBall point) = Path
  defIO disk = ipeDisk $ Disk (disk^.center) (disk^.squaredRadius)

instance (Radical r, Fractional r, Point_ point 2 r
         ) => HasDefaultIpeOut (BallByPoints' 3 point) where
  type DefaultIpeOut (BallByPoints' 3 point) = Path
  defIO disk = ipeDisk $ Disk (disk^.center) (disk^.squaredRadius)

instance (Radical r, Fractional r, Point_ point 2 r
         ) => HasDefaultIpeOut (DiskByPoints point) where
  type DefaultIpeOut (DiskByPoints point) = Path
  defIO disk = ipeDisk $ Disk (disk^.center) (disk^.squaredRadius)

instance Radical r => HasDefaultIpeOut (Circle (Point 2 r)) where
  type DefaultIpeOut (Circle (Point 2 r)) = Path
  defIO = ipeCircle

instance Num r => HasDefaultIpeOut (Rectangle (Point 2 r)) where
  type DefaultIpeOut (Rectangle (Point 2 r)) = Path
  defIO = ipeRectangle


instance HasDefaultIpeOut (Group r) where
  type DefaultIpeOut (Group r) = Group
  defIO = (:+ mempty)

instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (HalfSpaceF (LinePV 2 r)) where
  type DefaultIpeOut (HalfSpaceF (LinePV 2 r)) = Group
  defIO = ipeHalfPlane gray

instance (Fractional r, Ord r, Show r) => HasDefaultIpeOut (HalfSpaceF (LineEQ r)) where
  type DefaultIpeOut (HalfSpaceF (LineEQ r)) = Group
  defIO h = defIO $ h&boundingHyperPlaneLens %~ fromLineEQ

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
ipeLine :: (Ord r, Fractional r, Show r) => IpeOut (LinePV 2 r) Path r
ipeLine = ipeLineIn defaultBox




-- | Renders the line in the given box.
--
-- pre: the intersection of the box with the line is non-empty
ipeLineIn        :: forall r. (Ord r, Fractional r, Show r)
                 => Rectangle (Point 2 r) -> IpeOut (LinePV 2 r) Path r
ipeLineIn bBox l = case l `intersect` bBox of
  Nothing                         -> error "ipeLineIn: precondition failed, no intersection"
  Just (Line_x_Box_Point _)       -> error "ipeLineIn: precondition failed, single point"
  Just (Line_x_Box_LineSegment s) -> ipeLineSegment s

-- | Renders an Halfine.
--
-- pre: the intersection of the box with the line is non-empty
ipeHalfLine :: (Ord r, Fractional r, Point_ point 2 r, Show r, Show point) => IpeOut (HalfLine point) Path r
ipeHalfLine = \(HalfLine p v) -> ipeHalfLineIn defaultBox $ HalfLine (p^.asPoint) v

-- | Renders a ray, i.e. a half line drawing an arrow in the direction
-- of the ray.
--
-- pre: the intersection of the box with the line is non-empty
ipeRay :: (Ord r, Fractional r, Point_ point 2 r, Show r, Show point) => IpeOut (HalfLine point) Path r
ipeRay = \hl -> ipeHalfLine hl ! attr SArrow normalArrow

-- | Renders the HalfLine in the given box.
--
-- pre: the intersection of the box with the halfline is a line segment
ipeHalfLineIn         :: (Ord r, Fractional r, Point_ point 2 r, Show r, Show point)
                      => Rectangle point -> IpeOut (HalfLine point) Path r
ipeHalfLineIn bBox hl = case hl `intersect` bBox of
  Nothing                       -> error $ "ipeHalfLineIn: precondition failed, no intersection"
                                   <> " " <> show (hl,bBox)

  Just (HalfLine_x_Box_Point _) -> error "ipeHalfLineIn: precondition failed, single point"
  Just (HalfLine_x_Box_LineSegment seg) -> ipeLineSegment seg

-- | Renders an line segment to a Path
ipeLineSegment   :: (LineSegment_ lineSegment point, Point_ point 2 r)
                 => IpeOut lineSegment Path r
ipeLineSegment s = (path . pathSegment $ s) :+ mempty

-- | Renders a polyline to a Path
ipePolyLine   :: IpeOut (PolyLine (Point 2 r)) Path r
ipePolyLine p = (path . PolyLineSegment $ p) :+ mempty


-- | Renders an Ellipse to a Path
ipeEllipse :: IpeOut (Ellipse r) Path r
ipeEllipse = \e -> path (EllipseSegment e) :+ mempty

-- | Renders a circle to a Path
ipeCircle :: Radical r => IpeOut (Circle (Point 2 r)) Path r
ipeCircle = ipeEllipse . circleToEllipse

-- | Renders a Disk to a Path
ipeDisk   :: Radical r => IpeOut (Disk (Point 2 r)) Path r
ipeDisk d = ipeCircle (MkSphere d) ! attr SFill (IpeColor "0.722 0.145 0.137")

-- | Renders a Bezier curve to a Path
ipeBezier :: IpeOut (CubicBezier (Point 2 r)) Path r
ipeBezier b = (path $ CubicBezierSegment b) :+ mempty

-- | Helper to construct a path from a singleton item
path :: PathSegment r -> Path r
path = Path . Seq.singleton

-- | Construct a PolyLine path segment
pathSegment :: (LineSegment_ lineSegment point, Point_ point 2 r)
            => lineSegment -> PathSegment r
pathSegment = PolyLineSegment . fmap (^.asPoint) . lineSegmentToPolyLine
  where
    lineSegmentToPolyLine s = polyLineFromPoints . NonEmpty.fromList $ [s^.start, s^.end]

-- | Render as a polygon
ipePolygon    :: Polygon_ polygon point r => IpeOut polygon Path r
ipePolygon pg = Path (outer <| inners) :+ mempty
  where
    outer  = toPolygonPathSegment pg
    inners = toPolygonPathSegment <$> toSequenceOf holes pg
    toSequenceOf l = foldMapOf l Seq.singleton

-- | Helper type to build a path segment
toPolygonPathSegment :: ( HasOuterBoundary polygon, Point_ point 2 r, Vertex polygon ~ point)
                     => polygon -> PathSegment r
toPolygonPathSegment = PolygonPath AsIs . uncheckedFromCCWPoints
                     . toNonEmptyOf (outerBoundary.asPoint)
  -- this feels a bit silly, I feel we should directly be able to construct
  -- the polygon just using the outerBoundaryFold, but whatever.

-- | Draw a polygon
ipeSimplePolygon    :: (SimplePolygon_ simplePolygon point r)
                    => IpeOut simplePolygon Path r
ipeSimplePolygon pg = path (PolygonPath AsIs pg') :+ mempty
  where
    pg' = uncheckedFromCCWPoints $ toNonEmptyOf (vertices.asPoint) pg
  -- TODO, maybe write a 'toNonEmptyVectorOf' to avoid copying

-- | Draw a polygon
ipeSimplePolygon'    :: Foldable1 f => IpeOut (SimplePolygonF f (Point 2 r)) Path r
ipeSimplePolygon' pg = review' _asSimplePolygon pg :+ mempty

-- | A slightly more general version of review that allows the s and t to differ.
-- (and in some sense it is less general, since I don't care about monad constraints here)
review'   :: forall s t a b. Prism s t a b -> b -> t
review' p = review $ reviewing p
-- alternatively:
-- review' p = withPrism p (\f _ -> f)


-- | Draw a Rectangle
ipeRectangle   :: Num r => IpeOut (Rectangle (Point 2 r)) Path r
ipeRectangle r = ipeSimplePolygon' @NonEmptyVector . uncheckedFromCCWPoints . NonEmpty.fromList
               $ [tl,tr,br,bl]
  where
    Corners tl tr br bl = corners r

-- | Renders a polyline to a Path
ipeTriangle :: (Eq r, Num r) => IpeOut (Triangle (Point 2 r)) Path r
ipeTriangle = ipeSimplePolygon' @NonEmptyVector . uncheckedFromCCWPoints
            . toCounterClockwiseTriangle


-- | Default rendering of halfplanes
ipeHalfPlane :: (Show r, Fractional r, Ord r)
             => IpeColor r -> IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlane = ipeHalfPlaneIn defaultBox

-- | Draw a halfplane in the given rectangle.
--
-- We draw both the border (in black) and the interior (20% transparant gray) of the halfpace
ipeHalfPlaneIn            :: (Ord r, Fractional r, Show r)
                          => Rectangle (Point 2 r)
                          -> IpeColor r
                          -> IpeOut (HalfSpaceF (LinePV 2 r)) Group r
ipeHalfPlaneIn rect' c hl = case hl `intersect` rect' of
    Nothing -> ipeGroup [] -- this should not really happen I guess?
    Just is -> case is of
      ActualPolygon interior -> ipeGroup [ iO $ ipeSimplePolygon interior
                                              ! attr SFill c
                                              ! attr SOpacity (Text.pack "20%")
                                         , boundary
                                         ]
      _                      -> ipeGroup [ boundary ]
  where
    boundary = iO $ ipeLineIn rect' (hl^.boundingHyperPlane)


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
