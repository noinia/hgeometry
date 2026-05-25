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
import           HGeometry.Matrix (Matrix)
import qualified Ipe as Ipe
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
  toMisoString (RGB r g b) = mconcat [ "rgb("
                                     , mconcat . List.intersperse " " . map toMisoString $ [r,g,b]
                                     , ")"
                                     ]

--------------------------------------------------------------------------------
-- * Dealing with attributes

instance ToMisoString r => ToMisoString (IpeValue r) where
  toMisoString = \case
      Named t  -> toMisoString t
      Valued v -> toMisoString v

instance ToMisoString r => ToMisoString (Ipe.IpePen r) where
  toMisoString (Ipe.IpePen v) = toMisoString v
    -- TODO: only length or percentages are supposed to be supported.

instance ToMisoString r => ToMisoString (Ipe.IpeSize r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (Ipe.IpeArrow r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (Ipe.IpeDash r) where
  toMisoString _ = mempty

instance ToMisoString r => ToMisoString (Matrix 3 3 r) where
  toMisoString _ = mempty

instance ToMisoString Ipe.FillType where
  toMisoString _ = mempty

instance ToMisoString Ipe.PinType where
  toMisoString _ = mempty

instance ToMisoString Ipe.LineJoin where
  toMisoString = \case
    Ipe.Miter  -> "miter"
    Ipe.Round  -> "round"
    Ipe.Bevel  -> "bevel"
    -- arcs and MiterClip are not supported at the moment
    -- Arcs  -> "arcs"
    -- MiterClip -> "miter-clip"

instance ToMisoString Ipe.TransformationTypes where
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


instance ToMisoString r => ToMisoString (Ipe.TextSizeUnit r) where
  toMisoString (Ipe.TextSizeUnit x) = toMisoString x

instance ToMisoString Ipe.VerticalAlignment where
  toMisoString = \case
    Ipe.AlignTop      -> "top"
    Ipe.AlignVCenter  -> "center"
    Ipe.AlignBottom   -> "bottom"
    Ipe.AlignBaseline -> "baseline"

instance ToMisoString Ipe.HorizontalAlignment where
  toMisoString = \case
    Ipe.AlignLeft    -> "left"
    Ipe.AlignHCenter -> "center"
    Ipe.AlignRight   -> "right"
