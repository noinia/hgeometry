{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module HGeometry.Miso.Svg.Writer
  (
  ) where

import           Control.Lens hiding (Const,rmap)
import qualified Data.Foldable as F
import           Data.Maybe (catMaybes)
import           Data.Proxy
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Miso.OrphanInstances ()
-- import           HGeometry.PlanarSubdivision.Basic
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Foldable.Util
import           HGeometry.Vector
import qualified Ipe as Ipe
import qualified Ipe.Attributes as IA
import           Miso hiding (width_,height_,view)
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
-- * Default implementations for drawing geometric objects

-- | Default implementation for drawing geometric objects
class Drawable t where
  draw       :: t -> [Attribute action] -> View action
  draw x ats = drawWith x ats []

  drawWith          :: t -> [Attribute action] -> [View action] -> View action
  drawWith x ats _b = draw x ats

instance (Drawable l, Drawable r) => Drawable (Either l r) where
  draw = either draw draw

instance ToMisoString r => Drawable (Point 2 r) where
  draw = dPoint

instance (Point_ point 2 r, ToMisoString r, Num r) => Drawable (Rectangle point) where
  draw = dRectangle

instance ( Point_ point 2 r, EndPoint_ (endPoint point), IxValue (endPoint point) ~ point
         , ToMisoString r) => Drawable (LineSegment endPoint point) where
  draw = dLineSegment

instance (Point_ point 2 r, ToMisoString r) => Drawable (PolyLine point) where
  draw = dPolyLine

instance ( Point_ point 2 r, VertexContainer f point, HasFromFoldable1 f
         , ToMisoString r) => Drawable (SimplePolygonF f point) where
 draw = dSimplePolygon

instance ( Point_ point 2 r, VertexContainer f point, HasFromFoldable1 f
         , ToMisoString r) => Drawable (ConvexPolygonF f point) where
  draw = dSimplePolygon . toSimplePolygon

instance (Point_ point 2 r, ToMisoString r, Floating r) => Drawable (Circle point) where
  draw = dCircle

instance (Point_ point 2 r, ToMisoString r, Floating r) => Drawable (Disk point) where
  draw = dDisk

--------------------------------------------------------------------------------
-- * Functions to draw geometric objects


dPoint   :: (Point_ point 2 r, ToMisoString r) => point -> [Attribute action] -> View action
dPoint p = withAts ellipse_ [ cx_ (ms $ p^.xCoord), cy_ (ms $ p^.yCoord)
                            , rx_ "5", ry_ "5"
                            ]

dRectangle   :: ( Rectangle_ rectangle point, Point_ point 2 r, ToMisoString r, Num r)
             => rectangle -> [Attribute action] -> View action
dRectangle b = let Point2 x y  = over coordinates ms $ b^.minPoint.asPoint
                   Vector2 w h = ms <$> b^.to size
               in withAts rect_ [ x_ x, y_ y, width_ w, height_ h, fill_ "none"]

dSimplePolygon    :: (SimplePolygon_ simplePolygon point r, ToMisoString r)
                  => simplePolygon -> [Attribute action] -> View action
dSimplePolygon pg = withAts polygon_ [points_ $ toPointsString $ pg^..vertices ]


  -- \case
  --   SimplePolygon vs   ->
  --   MultiPolygon vs hs -> withAts path_ [d_ s]
  --     where
  --       s = mconcat . map toSimplePolygonPathString $ vs : hs


-- toSimplePolygonPathString                    :: ToMisoString r => SimplePolygon p r -> MisoString
-- toSimplePolygonPathString (SimplePolygon vs) = mconcat [ "M", toOp p
--                                                        , mconcat $ map (\q -> "L" <> toOp q) ps
--                                                        , "Z"
--                                                        ]
  -- where
  --   p :| ps = F1.toNonEmpty vs
  --   toOp (Point2 x y :+ _) = ms x <> " " <> ms y <> " "


dPolyLine    :: (PolyLine_ polyLine point, Point_ point 2 r, ToMisoString r)
             => polyLine -> [Attribute action] -> View action
dPolyLine pl = withAts polyline_ [points_ . toPointsString $ pl^..vertices ]

dLineSegment   :: ( LineSegment_ lineSegment point, Point_ point 2 r, ToMisoString r)
               => lineSegment -> [Attribute action] -> View action
dLineSegment s = withAts polyline_ [ points_ $ toPointsString [s^.start, s^.end] ]

-- | constructs a list of points to be used in the 'points' svg attribute.
toPointsString :: (Point_ point 2 r, ToMisoString r, Foldable f) => f point -> MisoString
toPointsString =
  MisoString.unwords . map (\(Point2_ x y) -> mconcat [ms x, ",", ms y]) . F.toList


dCircle              :: (Point_ point 2 r, ToMisoString r)
                     => Circle point -> [Attribute action] -> View action
dCircle (Circle c r) = withAts ellipse_ [ rx_ . ms $ r
                                         , ry_ . ms $ r
                                         , cx_ . ms $ c^.xCoord
                                         , cy_ . ms $ c^.yCoord
                                         , fill_ "none"
                                         ]

dDisk             :: (Disk_ disk point, Point_ point 2 r, ToMisoString r, Floating r)
                  => disk -> [Attribute action] -> View action
dDisk (Disk_ c r) = dCircle (Circle c r)

-- instance (ToMisoString r, Drawable v, Drawable  => Drawable (PlanarSubdivision s v e f r)


-- dPlanarSubdivision        :: PlanarSubdivision s (Maybe (View action))
--                                                  (Maybe (View action))
--                                                  (Maybe (View action)) r
--                           -> [Attribute action] -> View action
-- dPlanarSubdivision = dPlanarSubdivisionWith (^._2.vData) (^._2.extra) (^._2.extra)


-- -- | Draws only the values for which we have a Just attribute
-- dPlanarSubdivision' :: (ToMisoString r)
--                     => PlanarSubdivision s (Maybe (Ipe.IpeAttributes Ipe.IpeSymbol r))
--                                            (Maybe (Ipe.IpeAttributes Ipe.Path r))
--                                            (Maybe (Ipe.IpeAttributes Ipe.Path r)) r
--                     -> [Attribute action]
--                     -> View action
-- dPlanarSubdivision' = dPlanarSubdivisionWith fv fe ff
--   where
--     fv (_,v) = (\ats -> draw (v^.location) (svgWriteAttrs ats)) <$> v^.vData
--     fe (_,e) = (\ats -> draw (e^.core)     (svgWriteAttrs ats)) <$> e^.extra
--     ff (_,f) = (\ats -> draw (f^.core)     (svgWriteAttrs ats)) <$> f^.extra


-- type DrawF a action = a -> Maybe (View action)

-- dPlanarSubdivisionWith                 :: DrawF (VertexId' s, VertexData r v)          action
--                                        -> DrawF (Dart s,      LineSegment 2 v r :+ e)  action
--                                        -> DrawF (FaceId' s,   SomePolygon v r :+ f)    action
--                                        -> PlanarSubdivision s v e f r
--                                        -> [Attribute action]
--                                        -> View action
-- dPlanarSubdivisionWith fv fe ff ps ats = g_ ats (fs <> es <> vs)
--     -- draw faces at the bottom, then edges, and finally the vertices
--   where
--     vs = mapMaybe fv . F.toList . vertices        $ ps
--     es = mapMaybe fe . F.toList . edgeSegments    $ ps
--     fs = mapMaybe ff . F.toList . rawFacePolygons $ ps

--------------------------------------------------------------------------------
-- * Functions to draw ipe objects

instance ToMisoString r => Drawable (Ipe.IpeObject r) where
  draw = \case
    Ipe.IpeGroup g     -> draw g
    Ipe.IpeImage i     -> draw i
    Ipe.IpeTextLabel t -> draw t
    Ipe.IpeMiniPage m  -> draw m
    Ipe.IpeUse u       -> draw u
    Ipe.IpePath p      -> draw p

instance ( Drawable g
         , AllConstrained IpeToMisoAttr rs
         , ReifyConstraint ToMisoString (IA.Attr f) rs
         , RMap rs, RecordToList rs
         , RecAll (IA.Attr f) rs ToMisoString
         ) => Drawable (g :+ IA.Attributes f rs) where
  draw (i :+ iAts) ats = draw i (svgWriteAttrs iAts <> ats)

instance ToMisoString r => Drawable (Ipe.Group r) where
  draw (Ipe.Group os) ats = g_ ats (map (flip draw []) os)

instance ToMisoString r => Drawable (Ipe.Image r) where
instance ToMisoString r => Drawable (Ipe.TextLabel r) where
instance ToMisoString r => Drawable (Ipe.MiniPage r) where


instance ToMisoString r => Drawable (Ipe.IpeSymbol r) where
  draw (Ipe.Symbol p _ ) = withAts ellipse_ [ rx_ r
                                            , ry_ r
                                            , cx_ . ms $ p^.xCoord
                                            , cy_ . ms $ p^.yCoord
                                            ]
    where
      r = "5"

instance ToMisoString r => Drawable (Ipe.Path r) where
  draw (Ipe.Path s) ats = g_ [] (map (flip draw (fill_ "none":ats)) . F.toList $ s)
  -- svg fills paths by default; don't do that unless specified otherwise

instance ToMisoString r => Drawable (Ipe.PathSegment r) where
  draw = \case
    Ipe.PolyLineSegment pl -> dPolyLine pl
    Ipe.PolygonPath  pg    -> dSimplePolygon pg
    _                      -> error "toValue: not implemented yet"

--------------------------------------------------------------------------------
-- * Dealing with attributes



--------------------------------------------------------------------------------

type SvgF action = MisoString -> Attribute action

-- | Functon to write all attributes in a Rec
svgWriteAttrs              :: ( AllConstrained IpeToMisoAttr rs
                              , RMap rs, RecordToList rs
                              , ReifyConstraint ToMisoString (IA.Attr f) rs
                              , RecAll (IA.Attr f) rs ToMisoString
                              )
                           => IA.Attributes f rs
                           -> [Attribute action]
svgWriteAttrs (IA.Attrs r) = map (\(g,x) -> g x) . catMaybes . recordToList
                             $ IA.zipRecsWith f (writeAttrFunctions r)
                                                (writeAttrValues r)
  where
    f (Const mn) (Const mv) = Const $ (,) <$> mn <*> mv

-- | Writing Attribute names
writeAttrFunctions           :: AllConstrained IpeToMisoAttr rs
                             => Rec f rs
                             -> Rec (Const (Maybe (SvgF action))) rs
writeAttrFunctions RNil      = RNil
writeAttrFunctions (x :& xs) = Const (write'' x) :& writeAttrFunctions xs
  where
    write''   :: forall f s action. IpeToMisoAttr s => f s -> Maybe (SvgF action)
    write'' _ = attrSvg (Proxy :: Proxy s)


-- | Writing the attribute values
writeAttrValues :: ( ReifyConstraint ToMisoString (IA.Attr f) rs, RMap rs
                   , RecAll (IA.Attr f) rs ToMisoString)
                => Rec (IA.Attr f) rs -> Rec (Const (Maybe MisoString)) rs
writeAttrValues = rmap (\(Compose (Dict x)) -> Const $ toMaybeValue x)
                . reifyConstraint @ToMisoString

toMaybeValue   :: ToMisoString (IA.Attr f at) => IA.Attr f at -> Maybe MisoString
toMaybeValue a = case a of
                   IA.NoAttr -> Nothing
                   IA.Attr _ -> Just $ toMisoString a

-- | For the types representing attribute values we can get the name/key to use
-- when serializing to ipe.
class IpeToMisoAttr (a :: IA.AttributeUniverse) where
  attrSvg :: proxy a -> Maybe (SvgF action)

-- CommonAttributeUnivers
instance IpeToMisoAttr IA.Layer           where attrSvg _ = Nothing
instance IpeToMisoAttr IA.Matrix          where attrSvg _ = Nothing -- TODO
instance IpeToMisoAttr IA.Pin             where attrSvg _ = Nothing
instance IpeToMisoAttr IA.Transformations where attrSvg _ = Nothing

-- IpeSymbolAttributeUniversre
instance IpeToMisoAttr IA.Stroke       where attrSvg _ = Just stroke_
instance IpeToMisoAttr IA.Fill         where attrSvg _ = Just fill_
instance IpeToMisoAttr IA.Pen          where attrSvg _ = Nothing
instance IpeToMisoAttr IA.Size         where attrSvg _ = Nothing

-- PathAttributeUniverse
instance IpeToMisoAttr IA.Dash       where attrSvg _ = Nothing
instance IpeToMisoAttr IA.LineCap    where attrSvg _ = Just strokeLinecap_
instance IpeToMisoAttr IA.LineJoin   where attrSvg _ = Nothing
instance IpeToMisoAttr IA.FillRule   where attrSvg _ = Nothing
instance IpeToMisoAttr IA.Arrow      where attrSvg _ = Nothing
instance IpeToMisoAttr IA.RArrow     where attrSvg _ = Nothing
instance IpeToMisoAttr IA.StrokeOpacity where attrSvg _ = Just strokeOpacity_
instance IpeToMisoAttr IA.Opacity    where attrSvg _ = Just fillOpacity_
instance IpeToMisoAttr IA.Tiling     where attrSvg _ = Nothing
instance IpeToMisoAttr IA.Gradient   where attrSvg _ = Nothing

-- GroupAttributeUniverse
instance IpeToMisoAttr IA.Clip     where attrSvg _ = Nothing -- Just clipPath_
