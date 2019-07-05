{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.Geometry.Interactive.OrphanInstances where

import           Data.Colour.SRGB (RGB(..))
import           Data.Ext
import           Data.Fixed
import qualified Data.Geometry.Ipe as Ipe
import qualified Data.Geometry.Ipe.Attributes as IA
import           Data.Geometry.Ipe.Color (IpeColor(..))
import           Data.Geometry.Ipe.Value
import           Data.Geometry.Transformation (Matrix)
import qualified Data.List as List
import           Data.Singletons (Apply)
import           Miso.String (ToMisoString(..))

--------------------------------------------------------------------------------

instance HasResolution p => ToMisoString (Fixed p) where
  toMisoString = toMisoString . showFixed True
  fromMisoString = read . fromMisoString

instance ToMisoString Rational where
  toMisoString = toMisoString @Pico . realToFrac
  fromMisoString = realToFrac . fromMisoString @Pico

instance ToMisoString r => ToMisoString (RGB r) where
  toMisoString (RGB r g b) = mconcat . List.intersperse " " . map toMisoString $ [r,g,b]
  fromMisoString = undefined

--------------------------------------------------------------------------------
-- * Dealing with attributes

instance ToMisoString (Apply f at) => ToMisoString (IA.Attr f at) where
  toMisoString att = maybe "" toMisoString $ IA._getAttr att
  fromMisoString = IA.Attr . fromMisoString

instance ToMisoString r => ToMisoString (IpeValue r) where
  toMisoString = \case
      Named t  -> toMisoString t
      Valued v -> toMisoString v
  fromMisoString = undefined

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
  toMisoString (IpeColor c) = toMisoString c
  fromMisoString = undefined

instance ToMisoString r => ToMisoString (Ipe.Path r) where
  toMisoString _ = mempty
  fromMisoString = undefined
    -- FIXME: This does not actually show the path

instance ToMisoString Ipe.LayerName where
  toMisoString (Ipe.LayerName t) = toMisoString t
  fromMisoString = Ipe.LayerName . fromMisoString
