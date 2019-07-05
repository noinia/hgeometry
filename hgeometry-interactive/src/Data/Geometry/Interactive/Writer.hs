{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Geometry.Interactive.Writer where

import           Control.Lens hiding (Const,rmap)
import qualified Data.ByteString as B
import           Data.Colour.SRGB (RGB(..))
import           Data.Ext
import           Data.Fixed
import qualified Data.Foldable as F
import           Data.Geometry.Ball
import           Data.Geometry.Interactive.StaticCanvas
import qualified Data.Geometry.Ipe as Ipe
import qualified Data.Geometry.Ipe.Attributes as IA
import           Data.Geometry.Ipe.Color (IpeColor(..))
import           Data.Geometry.Ipe.Value
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.Geometry.Transformation (Matrix)
import           Data.Geometry.Vector
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Proxy
import qualified Data.Semigroup.Foldable as F1
import           Data.Singletons (Apply)
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
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
cPolygon (MultiPolygon vs hs) = withAts path_ [d_ s]
  where
    s = mconcat . map toSimplePolygonPathString $ SimplePolygon vs : hs

toSimplePolygonPathString                    :: ToMisoString r => SimplePolygon p r -> MisoString
toSimplePolygonPathString (SimplePolygon vs) = mconcat [ "M", toOp p
                                                       , mconcat $ map (\q -> "L" <> toOp q) ps
                                                       , "Z"
                                                       ]
  where
    p :| ps = F1.toNonEmpty vs
    toOp (Point2 x y :+ _) = ms x <> " " <> ms y <> " "


cPolyLine    :: ToMisoString r => PolyLine 2 p r -> [Attribute action] -> View action
cPolyLine pl = withAts polyline_ [points_ . toPointsString $ pl^.points ]

cLineSegment   :: ToMisoString r => LineSegment 2 p r -> [Attribute action] -> View action
cLineSegment s = cPolyLine (fromLineSegment s)


-- | constructs a list of points to be used in the 'points' svg attribute.
toPointsString :: (ToMisoString r, Foldable f) => f (Point 2 r :+ p) -> MisoString
toPointsString =
  MisoString.unwords . map (\(Point2 x y :+ _) -> mconcat [ms x, ",", ms y]) . F.toList



cCircle              :: ToMisoString r
                     => Circle p r -> [Attribute action] -> View action
cCircle (Circle c r) = withAts ellipse_ [ rx_ . ms $ r
                                        , ry_ . ms $ r
                                        , cx_ . ms $ c^.core.xCoord
                                        , cy_ . ms $ c^.core.yCoord
                                        , fill_ "none"
                                        ]

cDisk            :: (ToMisoString r, Floating r)
                 => Disk p r -> [Attribute action] -> View action
cDisk (Disk c r) = cCircle (Circle c r)


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
    Ipe.PolyLineSegment pl -> cPolyLine pl
    Ipe.PolygonPath  pg    -> cPolygon pg
    _                      -> error "toValue: not implemented yet"


--------------------------------------------------------------------------------
-- * Dealing with attributes

instance ToMisoString (Apply f at) => ToMisoString (IA.Attr f at) where
  toMisoString att = maybe "" toMisoString $ IA._getAttr att
  fromMisoString = IA.Attr . fromMisoString

instance ToMisoString r => ToMisoString (IA.IpePen r) where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (IA.IpeSize r) where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (IA.IpeArrow r) where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (IA.IpeDash r) where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (Matrix 3 3 r) where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString IA.FillType where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString IA.PinType where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString IA.TransformationTypes where
  toMisoString _ = mempty
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (IpeColor r) where
  toMisoString (IpeColor c) = case c of
      Named t  -> toMisoString t
      Valued v -> toMisoString v
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (RGB r) where
  toMisoString (RGB r g b) = mconcat . List.intersperse " " . map toMisoString $ [r,g,b]
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (Ipe.Path r) where
  toMisoString _ = mempty
  fromMisoString = undefined
    -- FIXME: This does not actually show the path

instance ToMisoString Ipe.LayerName where
  toMisoString (Ipe.LayerName t) = toMisoString t
  fromMisoString = Ipe.LayerName . fromMisoString


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
instance IpeToMisoAttr IA.Opacity    where attrSvg _ = Just fillOpacity_
instance IpeToMisoAttr IA.Tiling     where attrSvg _ = Nothing
instance IpeToMisoAttr IA.Gradient   where attrSvg _ = Nothing

-- GroupAttributeUniverse
instance IpeToMisoAttr IA.Clip     where attrSvg _ = Nothing -- Just clipPath_
