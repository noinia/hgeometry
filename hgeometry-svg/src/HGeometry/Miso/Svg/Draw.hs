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

import           Data.List.NonEmpty (NonEmpty(..))
import           Control.Lens
import           Data.Kind (Type)
import           HGeometry.PolyLine
import           HGeometry.Point
import           HGeometry.LineSegment
import           Data.Default
import           Ipe.Attributes
import           Ipe.Draw
import           Ipe.Content
import           Miso (View)
import           Data.Void
import           HGeometry.Polygon
import           HGeometry.Properties
import qualified HGeometry.Miso.Svg as Svg
import           Miso.String (ToMisoString(..), ms, intercalate)
import qualified Miso.Svg as Elem
import qualified Miso.Svg.Property as Prop
import           HGeometry.BezierSpline
import qualified Miso

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
  draw ats p = [ Elem.ellipse_ ([ Prop.cx_ (ms $ p^.xCoord), Prop.cy_ (ms $ p^.yCoord)
                                , Prop.rx_ "2", Prop.ry_ "2"
                                ] <> Svg.svgWriteAttrs (apply ats))
               ]

instance (Point_ point 2 r, Fractional r, ToMisoString r, r ~ NumType point
         ) => IsDrawable (Svg model action) (CubicBezier point) where
  type AttrOf (Svg model action) (CubicBezier point) = PathAttributes (NumType point)
  draw ats (BezierSpline vs) = [ Elem.path_ ([ Prop.d_ str
                                             ] <> Svg.svgWriteAttrs (apply ats))
                               ]
    where
      str = let v :| rest = toNonEmptyOf (traversed1.asPoint) vs
            in "M " <> toStr v <> " C " <> intercalate ", " [ toStr w | w <- rest ]
      toStr (Point2 x y) = ms x <> " " <> ms y


instance ToMisoString r => IsDrawable (Svg model action) (TextLabel r) where
  type AttrOf (Svg model action) (TextLabel r) = TextAttributes r
  draw ats (Label txt loc) = [ Elem.text_ ([ Prop.x_ (loc^.xCoord.to ms)
                                           , Prop.y_ (loc^.yCoord.to ms)
                                           ] <> Svg.svgWriteAttrs (apply ats)
                                          )
                                          [ Miso.text $ ms txt]
                             ]
  -- TODO: I think I should just not use TextAttributes, but a custom
  -- SvgTextAttributes type


-- | Helper function to apply attributes
apply :: Default at => [at -> at] -> at
apply = foldl' (flip ($)) def
