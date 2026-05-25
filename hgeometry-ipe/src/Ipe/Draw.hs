{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Draw
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Class and setup for rendering/drawing
--
--------------------------------------------------------------------------------
module Ipe.Draw
  ( Rendered
  , Attr
  , IsDrawable(..)

  -- * The Ipe backend
  , Ipe
  ) where

import Data.Default
import Data.Text (Text)
import HGeometry.Ext
import HGeometry.Point
import Control.Lens
import Data.Kind (Type)
import Ipe.Types
import Ipe.FromIpe
import Ipe.Attributes
import HGeometry.Polygon
import Data.List.NonEmpty (NonEmpty)

--------------------------------------------------------------------------------

-- | The type of objects a backend renders
type family Rendered backend :: Type


-- | An Attribute Assignment
type Attr backend geom = AttrOf backend geom -> AttrOf backend geom

-- | A class that expresses that something is drawable using a particular backend
class ( Monoid (Rendered backend)
      ) => IsDrawable backend geom where

  -- | A GADT that expresses possible attributes for a particular object
  type AttrOf backend geom :: Type

  -- | Draw some objects
  draw :: [Attr backend geom] -> geom -> Rendered backend

instance ( IsDrawable backend a
         ) => IsDrawable backend (NonEmpty a) where
  type AttrOf backend (NonEmpty a) = AttrOf backend a
  draw ats = foldMap (draw @backend ats)

instance ( IsDrawable backend a
         ) => IsDrawable backend [a] where
  type AttrOf backend [a] = AttrOf backend a
  draw ats = foldMap (draw @backend ats)

--------------------------------------------------------------------------------
-- * Ipe Backend utils

-- | The Ipe backend
type data Ipe (r :: Type)

type instance Rendered (Ipe r) = [IpeObject r]

instance IsDrawable (Ipe r) (IpeObject r) where
  type AttrOf (Ipe r) (IpeObject r) = CommonAttributes r Maybe
  draw ats o = [ foldl' (\o' f -> o'&commonAttributes %~ f) o ats ]

instance IsDrawable (Ipe r) (Path r) where
  type AttrOf (Ipe r) (Path r) = PathAttributes r
  draw ats p = [ IpePath (p :+ mkAttrs ats) ]

instance IsDrawable (Ipe r) (IpeSymbol r) where
  type AttrOf (Ipe r) (IpeSymbol r) = SymbolAttributes r
  draw ats p = [ IpeUse (p :+ mkAttrs ats) ]

instance ( Point_ vertex 2 r, VertexContainer f vertex, Num r
         ) => IsDrawable (Ipe r) (SimplePolygonF f vertex) where
  type AttrOf (Ipe r) (SimplePolygonF f vertex) = PathAttributes r
  draw ats pg = draw @(Ipe r) ats (review _asSimplePolygon pg')
    where
      pg' = uncheckedFromCCWPoints $ toNonEmptyOf (vertices.asPoint) pg

--------------------------------------------------------------------------------

instance IsDrawable (Ipe r) (Point 2 r) where
  type AttrOf (Ipe r) (Point 2 r) = SymbolAttributes r
  draw ats p = [ IpeUse $ ipeDiskMark p & attributes %~ applyAttrs ats ]

-- | Create an ipe mark
ipeMark     :: Text -> Point 2 r -> IpeSymbol r :+ SymbolAttributes r
ipeMark n p = Symbol p n :+ def

-- | Creates a disk ipe mark
ipeDiskMark :: Point 2 r -> IpeSymbol r :+ SymbolAttributes r
ipeDiskMark = ipeMark "mark/disk(sx)"
