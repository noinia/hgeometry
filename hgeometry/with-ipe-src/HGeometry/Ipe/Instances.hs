{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ipe.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Some instances for HasDefaultIpeOut for geometry types that require
-- basic data types (in kernel) as well as polygons or polylines.
--
--------------------------------------------------------------------------------
module HGeometry.Ipe.Instances
  () where

import           HGeometry.Cone.Intersection
import           HGeometry.Cone
import qualified HGeometry.Slab as Slab
import           HGeometry.Ext
import           HGeometry.Kernel
import           Ipe
import           Ipe.Color
import           Control.Lens
import           HGeometry.HalfSpace
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Cyclic
import           HGeometry.Polygon

--------------------------------------------------------------------------------

instance (
         ) => HasDefaultIpeOut (Slab.Slab r side) where
  type DefaultIpeOut (Slab.Slab r side) = Group
  defIO _s = error "drawing slab not implemented yet"



instance (Point_ point 2 r, Fractional r, Ord r, Show r
         ) => HasDefaultIpeOut (Cone r point edge) where
  type DefaultIpeOut (Cone r point edge) = Group
  defIO c = ipeGroup [ iO $ defIO poly ! attr SFill blue
                     , iO $ ipeRay hl
                     , iO $ ipeRay hr
                     ]
    where
      hl = leftBoundary c'  ^.core
      hr = rightBoundary c' ^.core

      poly :: ConvexPolygonF (Cyclic NonEmpty) (Point 2 r)
      poly = toConvexPolygonIn rect c'&vertices %~ (^.asPoint)
      c' :: Cone r (Point 2 r) edge
      c' = c&apex %~ (^.asPoint)
      rect :: Rectangle (Point 2 r)
      rect = boundingBox $ defaultBox :| [boundingBox (c'^.apex) ]
      -- we have to include the bounding box of the apex since toConvexPolygonIn
      -- requires the apex to be in the box.

instance ( HasDefaultIpeOut line, Fractional r, Ord r, Show r
         , DefaultIpeOut line ~ Path
         , NumType line ~ r
         ) => HasDefaultIpeOut (LineHalfPlaneIntersection r line) where
  type DefaultIpeOut (LineHalfPlaneIntersection r line) = Path
  defIO = \case
    Line_x_HalfPlane_Line line   -> defIO line
    Line_x_HalfPlane_HalfLine hl -> defIO hl

instance ( HasDefaultIpeOut halfPlane
         , HasDefaultIpeOut (BoundingHyperPlane halfPlane 2 r)
         , HalfPlane_ halfPlane r
         , Fractional r, Ord r
         , Show r
         -- , DefaultIpeOut halfPlane ~ Path
         , NumType halfPlane ~ r
         , Dimension halfPlane ~ 2
         , Dimension (BoundingHyperPlane halfPlane 2 r) ~ 2
         , NumType (BoundingHyperPlane halfPlane 2 r) ~ r
         ) => HasDefaultIpeOut (HalfPlaneIntersection r halfPlane) where
  type DefaultIpeOut (HalfPlaneIntersection r halfPlane) = Group
  defIO = \case
    HalfPlane_x_HalfPlane_Line line           -> ipeGroup [iO $ defIO line]
    HalfPlane_x_HalfPlane_Slab slab           -> ipeGroup [iO $ defIO slab]
    HalfPlane_x_HalfPlane_Cone cone           -> ipeGroup [iO $ defIO cone]
    HalfPlane_x_HalfPlane_HalfPlane halfPlane -> ipeGroup [iO $ defIO halfPlane ]
