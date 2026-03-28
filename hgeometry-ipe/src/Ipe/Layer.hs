--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Layer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Layers in Ipe documents.
--
--------------------------------------------------------------------------------
module Ipe.Layer(
  LayerName(LayerName), layerName
  ) where

import           Control.Lens
import           Data.Text (Text)
import           GHC.Exts

--------------------------------------------------------------------------------

-- | Defines an Layer in Ipe.
newtype LayerName = LayerName {_layerName :: Text } deriving (Show,Read,Eq,Ord,IsString)

layerName :: Iso' LayerName Text
layerName = iso _layerName LayerName
{-# INLINE layerName #-}
