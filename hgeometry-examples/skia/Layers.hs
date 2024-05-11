{-# LANGUAGE TemplateHaskell            #-}
module Layers
  ( LayerStatus(..)
  , toggleStatus

  , Layer(Layer)
  , HasLayer(..)

  , Layers(Layers)
  , HasLayers(..)
  , allLayers
  ) where

import           Control.Lens hiding (view, element)
import           Miso
import           Miso.String (MisoString,ToMisoString(..), ms)

--------------------------------------------------------------------------------

data LayerStatus = Hidden | Visible
  deriving (Show,Eq)

toggleStatus = \case
  Hidden  -> Visible
  Visible -> Hidden

data Layer = Layer { _name :: MisoString
                   , _status :: LayerStatus
                   }
             deriving (Show,Eq)
makeClassy ''Layer

data Layers = Layers { _beforeActive :: [Layer] -- stored in reverse, so a zipper
                     , _activeLayer  :: Layer
                     , _afterActive  :: [Layer]
                     }
            deriving (Show,Eq)

allLayers                              :: Layers -> [Layer]
allLayers (Layers before active after) = reverse before <> [active] <> after

makeClassy ''Layers
