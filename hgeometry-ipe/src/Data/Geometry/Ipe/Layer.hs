{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Ipe.Layer(
  LayerName(LayerName), layerName
  ) where

import           Control.Lens
import           Data.Text (Text)
import           GHC.Exts

--------------------------------------------------------------------------------

newtype LayerName = LayerName {_layerName :: Text } deriving (Show,Read,Eq,Ord,IsString)
makeLenses ''LayerName
