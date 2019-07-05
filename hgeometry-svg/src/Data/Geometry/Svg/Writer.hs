{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Svg.Writer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :
--
-- Write geometry to svg
--
--------------------------------------------------------------------------------
module Data.Geometry.Svg.Writer where

import           Control.Lens hiding (rmap, Const(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Ext
import           Data.Fixed
import qualified Data.Foldable as F
import qualified Data.Geometry.Ipe.Attributes as IA
import           Data.Geometry.Ipe.Color (IpeColor(..))
import           Data.Geometry.Ipe.Types
import qualified Data.Geometry.Ipe.Types as Ipe
import           Data.Geometry.Ipe.Value (IpeValue(..))
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon
import           Data.Geometry.Svg.MathCoordinateSystem
import           Data.Geometry.Transformation (Matrix)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Monoid (mconcat)
import           Data.Proxy
import           Data.Ratio
import           Data.Semigroup.Foldable (toNonEmpty)
import           Data.Singletons (Apply)
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           Text.Blaze (ToMarkup(toMarkup), ToValue(toValue))
import qualified Text.Blaze.Svg as Svg
import qualified Text.Blaze.Svg.Renderer.Utf8 as SvgRender
import           Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as Svg
import qualified Text.Blaze.Svg11.Attributes as A

--------------------------------------------------------------------------------

-- | Converts an element into a valid svg document (including doctype etc.)
-- The size of the resulting svg is set to 800x600. Moreover, we flip the axes
-- so that the origin is in the bottom-left.
--
--
toSvgXML :: ToMarkup t => t -> B.ByteString
toSvgXML = SvgRender.renderSvg
         . Svg.docTypeSvg
         . renderCanvas (createCanvas @Double 800 600) []
         . svgO

-- | Convert an element to Svg using 'toSvgXML' and prints the resulting svg
-- (as xml) output to stdout.
--
printSvgXML :: ToMarkup t => t -> IO ()
printSvgXML = B8.putStrLn . toSvgXMLElem


-- | Convert an element to Svg
svgO :: ToMarkup a => a -> Svg.Svg
svgO = Svg.toSvg


-- | Convert an element to Svg, and render this svg as xml. Note that the xml
-- contains *only* this element.
toSvgXMLElem :: ToMarkup t => t -> B.ByteString
toSvgXMLElem = SvgRender.renderSvg . Svg.toSvg

-- | Convert an element to Svg, and prints the xml output to stdout.
printSvgXMLElem :: ToMarkup t => t -> IO ()
printSvgXMLElem = B8.putStrLn . toSvgXMLElem


--------------------------------------------------------------------------------

instance Real r => ToMarkup (IpeObject r) where
  toMarkup (IpeGroup g)         = toMarkup g
  toMarkup (IpeImage i)         = toMarkup i
  toMarkup (IpeTextLabel t)     = toMarkup t
  toMarkup (IpeMiniPage m)      = toMarkup m
  toMarkup (IpeUse u)           = toMarkup u
  toMarkup (IpePath (p :+ ats)) = toMarkup $ p :+ (ats' <> ats)
    where
      ats' = IA.attr IA.SFill $ IpeColor $ Named "transparent"
      -- svg assumes that by default the filling is set to transparent
      -- so make sure we do that as well

instance ( ToMarkup g
         , AllConstrained IpeToSvgAttr rs
         , ReifyConstraint ToValue (IA.Attr f) rs
         , RMap rs, RecordToList rs
         , RecAll (IA.Attr f) rs ToValue
         ) => ToMarkup (g :+ IA.Attributes f rs) where
  toMarkup (i :+ ats) = toMarkup i `applyAts` svgWriteAttrs ats

instance Real r => ToMarkup (TextLabel r) where
  toMarkup (Label t p) = text_ p [] t

instance Real r => ToMarkup (MiniPage r) where
  toMarkup (MiniPage t p w) = text_ p [A.width (toPValue w)] t

instance Real r => ToMarkup (Image r) where
  toMarkup _ = error "ToMarkup: Image not implemented yet"
  -- toMarkup (Image i r) = Svg.image t ! A.xlinkHref (toAValue i)
  --                                        ! A.y     (toPValue $ p^.yCoord)
  --                                        ! A.width (toPValue w)



instance HasResolution p => ToValue (Fixed p) where
  toValue = toAValue

instance Integral a => ToValue (Ratio a) where
  toValue = toValue @Pico . realToFrac

instance Real r => ToValue (PathSegment r) where
  toValue = \case
    PolyLineSegment pl -> Svg.mkPath . toPath $ pl^.points.to toNonEmpty
    PolygonPath  pg    -> Svg.mkPath $ do toPath $ pg^.outerBoundary.to toNonEmpty
                                          Svg.z
    EllipseSegment _   -> undefined
    _                  -> error "toValue: not implemented yet"

toPath     :: Real r => NonEmpty (Point 2 r :+ p) -> Svg.Path
toPath pts = case (^.core) <$> pts of
    (v:|vs) -> do Svg.m (showP $ v^.xCoord) (showP $ v^.yCoord)
                  mapM_ (\(Point2 x y) -> Svg.l (showP x) (showP y)) vs


instance Real r => ToMarkup (Ipe.Path r) where
  toMarkup p = Svg.path ! A.d (toValue p)

instance Real r => ToValue (Path r) where
  toValue (Path s) = mconcat . map toValue . F.toList $ s

instance Real r => ToMarkup (Ipe.IpeSymbol r) where
  toMarkup (Symbol p _) = Svg.circle ! A.cx (toPValue $ p^.xCoord)
                                     ! A.cy (toPValue $ p^.yCoord)
                                     ! A.r  (toPValue 5)
    -- TODO: for now just draw a disk of fixed radius

instance Real r => ToMarkup (Ipe.Group r) where
  toMarkup (Group os) = Svg.g (mapM_ toMarkup os)

--------------------------------------------------------------------------------
-- * Dealing with attributes

instance ToValue (Apply f at) => ToValue (IA.Attr f at) where
  toValue att = maybe mempty toValue $ IA._getAttr att


applyAts    :: Svg.Markup -> [(SvgF, Svg.AttributeValue)] -> Svg.Markup
applyAts x0 = F.foldl' (\x (f,v) -> x ! f v) x0

-- | Functon to write all attributes in a Rec
svgWriteAttrs              :: ( AllConstrained IpeToSvgAttr rs
                              , RMap rs, RecordToList rs
                              , ReifyConstraint ToValue (IA.Attr f) rs
                              , RecAll (IA.Attr f) rs ToValue
                              )
                           => IA.Attributes f rs
                           -> [(SvgF, Svg.AttributeValue)]
svgWriteAttrs (IA.Attrs r) = catMaybes . recordToList $ IA.zipRecsWith f (writeAttrFunctions r)
                                                                         (writeAttrValues r)
  where
    f (Const mn) (Const mv) = Const $ (,) <$> mn <*> mv

-- | Writing Attribute names
writeAttrFunctions           :: AllConstrained IpeToSvgAttr rs
                             => Rec f rs
                             -> Rec (Const (Maybe SvgF)) rs
writeAttrFunctions RNil      = RNil
writeAttrFunctions (x :& xs) = Const (write'' x) :& writeAttrFunctions xs
  where
    write''   :: forall f s. IpeToSvgAttr s => f s -> Maybe SvgF
    write'' _ = attrSvg (Proxy :: Proxy s)


-- | Writing the attribute values
writeAttrValues :: ( ReifyConstraint ToValue (IA.Attr f) rs, RMap rs
                   , RecAll (IA.Attr f) rs ToValue)
                => Rec (IA.Attr f) rs -> Rec (Const (Maybe Svg.AttributeValue)) rs
writeAttrValues = rmap (\(Compose (Dict x)) -> Const $ toMaybeValue x)
                . reifyConstraint @ToValue

toMaybeValue   :: ToValue (IA.Attr f at) => IA.Attr f at -> Maybe Svg.AttributeValue
toMaybeValue a = case a of
                   IA.NoAttr -> Nothing
                   IA.Attr _ -> Just $ toValue a

type SvgF = Svg.AttributeValue -> Svg.Attribute



-- | For the types representing attribute values we can get the name/key to use
-- when serializing to ipe.
class IpeToSvgAttr (a :: IA.AttributeUniverse) where
  attrSvg :: proxy a -> Maybe SvgF

-- CommonAttributeUnivers
instance IpeToSvgAttr IA.Layer           where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Matrix          where attrSvg _ = Nothing -- TODO
instance IpeToSvgAttr IA.Pin             where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Transformations where attrSvg _ = Nothing

-- IpeSymbolAttributeUniversre
instance IpeToSvgAttr IA.Stroke       where attrSvg _ = Just A.stroke
instance IpeToSvgAttr IA.Fill         where attrSvg _ = Just A.fill
instance IpeToSvgAttr IA.Pen          where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Size         where attrSvg _ = Nothing

-- PathAttributeUniverse
instance IpeToSvgAttr IA.Dash       where attrSvg _ = Nothing
instance IpeToSvgAttr IA.LineCap    where attrSvg _ = Just A.strokeLinecap
instance IpeToSvgAttr IA.LineJoin   where attrSvg _ = Nothing
instance IpeToSvgAttr IA.FillRule   where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Arrow      where attrSvg _ = Nothing
instance IpeToSvgAttr IA.RArrow     where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Opacity    where attrSvg _ = Just A.opacity
instance IpeToSvgAttr IA.Tiling     where attrSvg _ = Nothing
instance IpeToSvgAttr IA.Gradient   where attrSvg _ = Nothing

-- GroupAttributeUniverse
instance IpeToSvgAttr IA.Clip     where attrSvg _ = Just A.clip



--------------------------------------------------------------------------------

deriving instance ToValue LayerName

instance Real r => ToValue (IpeColor r) where
  toValue (IpeColor c) = case c of
                           Named t  -> toValue t
                           Valued v -> toAValue $ fmap showP v

-- TODO:



instance Real r => ToValue (IA.IpePen r) where
  toValue _ = mempty

instance Real r => ToValue (IA.IpeSize r) where
  toValue _ = mempty

instance Real r => ToValue (IA.IpeArrow r) where
  toValue _ = mempty

instance Real r => ToValue (IA.IpeDash r) where
  toValue _ = mempty

instance Real r => ToValue (Matrix 3 3 r) where
  toValue _ = mempty

instance ToValue IA.FillType where
  toValue _ = mempty

instance ToValue IA.PinType where
  toValue _ = mempty

instance ToValue IA.TransformationTypes where
  toValue _ = mempty
