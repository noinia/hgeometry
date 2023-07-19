{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Miso.OrphanInstances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Orpthan instances for To/From MisoString instances for several
-- geometric types.
--
--------------------------------------------------------------------------------
module HGeometry.Miso.OrphanInstances
  () where

import           Data.Colour.SRGB (RGB(..))
import           Data.Fixed
import qualified Data.List as List
import           Data.Singletons (Apply)
import           HGeometry.Matrix (Matrix)
import qualified Ipe as Ipe
import qualified Ipe.Attributes as IA
import           Ipe.Color (IpeColor(..))
import           Ipe.Value
import           Miso.String (ToMisoString(..), FromMisoString(..))

--------------------------------------------------------------------------------

instance HasResolution p => ToMisoString (Fixed p) where
  toMisoString = toMisoString . showFixed True

instance HasResolution p => FromMisoString (Fixed p) where
  fromMisoStringEither = fmap realToFrac . fromMisoStringEither @Double

instance ToMisoString Rational where
  toMisoString = toMisoString @Pico . realToFrac

instance FromMisoString Rational where
  fromMisoStringEither = fmap realToFrac . fromMisoStringEither @Pico

instance ToMisoString r => ToMisoString (RGB r) where
  toMisoString (RGB r g b) = mconcat . List.intersperse " " . map toMisoString $ [r,g,b]


--------------------------------------------------------------------------------
-- * Dealing with attributes

instance ToMisoString (Apply f at) => ToMisoString (IA.Attr f at) where
  toMisoString att = maybe "" toMisoString $ IA._getAttr att

instance FromMisoString (Apply f at) => FromMisoString (IA.Attr f at) where
  fromMisoStringEither = fmap IA.Attr . fromMisoStringEither

instance ToMisoString r => ToMisoString (IpeValue r) where
  toMisoString = \case
      Named t  -> toMisoString t
      Valued v -> toMisoString v

instance ToMisoString r => ToMisoString (IA.IpePen r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (IA.IpeSize r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (IA.IpeArrow r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (IA.IpeDash r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (Matrix 3 3 r) where
  toMisoString _ = mempty

instance ToMisoString IA.FillType where
  toMisoString _ = mempty

instance ToMisoString IA.PinType where
  toMisoString _ = mempty

instance ToMisoString IA.TransformationTypes where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (IpeColor r) where
  toMisoString (IpeColor c) = toMisoString c

instance ToMisoString r => ToMisoString (Ipe.Path r) where
  toMisoString _ = mempty
    -- FIXME: This does not actually show the path

instance ToMisoString Ipe.LayerName where
  toMisoString (Ipe.LayerName t) = toMisoString t

instance FromMisoString Ipe.LayerName where
  fromMisoStringEither = fmap Ipe.LayerName . fromMisoStringEither
