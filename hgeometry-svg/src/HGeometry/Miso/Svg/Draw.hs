{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE QuantifiedConstraints          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Miso.Svg.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Draw geometric objects to Svg files through miso.
--
-- This uses the newer API common with ipe
--
--------------------------------------------------------------------------------
module HGeometry.Miso.Svg.Draw
  ( Svg
  , SVG
  , module Ipe.Draw
  ) where

import           Control.Lens
import           Data.Kind (Type)
import           HGeometry.PolyLine
import           HGeometry.Point
import           HGeometry.LineSegment
import           Data.Default
import           Ipe.Attributes
import           Ipe.Draw
import           Miso (View)
import           Data.Void
import           HGeometry.Polygon
import           HGeometry.Properties
import qualified HGeometry.Miso.Svg as Svg
import           Miso.String (ToMisoString(..))

--------------------------------------------------------------------------------

-- | The Svg backend; which renders to Svg using Miso
type data Svg (model :: Type) (action :: Type)

-- | Static Svg
type SVG = Svg () Void

type instance Rendered (Svg model action) = [View model action]

--------------------------------------------------------------------------------

instance ( Point_ vertex 2 (NumType vertex)
         , ToMisoString (NumType vertex)
         , SimplePolygon_ (SimplePolygonF f vertex) vertex (NumType vertex)
         ) => IsDrawable (Svg model action) (SimplePolygonF f vertex) where
  type AttrOf (Svg model action) (SimplePolygonF f vertex) = PathAttributes (NumType vertex)
  draw ats poly = [ Svg.dSimplePolygon poly (Svg.svgWriteAttrs $ apply ats)
                  ]

instance ( Point_ vertex 2 (NumType vertex)
         , ToMisoString (NumType vertex)
         , PolyLine_ (PolyLineF f vertex) vertex
         ) => IsDrawable (Svg model action) (PolyLineF f vertex) where
  type AttrOf (Svg model action) (PolyLineF f vertex) = PathAttributes (NumType vertex)
  draw ats poly = [ Svg.dPolyLine poly (Svg.svgWriteAttrs $ apply ats) ]


instance ( Point_ vertex 2 (NumType vertex)
         , ToMisoString (NumType vertex)
         , EndPoint_ (endPoint vertex), IxValue (endPoint vertex) ~ vertex
         ) => IsDrawable (Svg model action) (LineSegment endPoint vertex) where
  type AttrOf (Svg model action) (LineSegment endPoint vertex) = PathAttributes (NumType vertex)
  draw ats seg = [ Svg.dLineSegment seg (Svg.svgWriteAttrs $ apply ats) ]


instance ( ToMisoString r
         ) => IsDrawable (Svg model action) (Point 2 r) where
  type AttrOf (Svg model action) (Point 2 r) = SymbolAttributes r
  draw ats p = [ Svg.dPoint p (Svg.svgWriteAttrs $ apply ats)]

-- | Helper function to apply attributes
apply :: Default at => [at -> at] -> at
apply = foldl' (flip ($)) def
