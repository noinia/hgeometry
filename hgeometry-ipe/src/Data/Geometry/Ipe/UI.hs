module Data.Geometry.Ipe.UI where

import Data.Geometry.Ipe.Types

data UIAttributes = UIAtributes deriving (Show,Eq,Ord)

data Snap = Snap deriving (Show,Eq,Ord)

data UI r = UI { _document          :: IpeFile r
               , _currentPageNumber :: Int
               , _currentViewNumber :: Int
               , _attributes        :: UIAttributes
               , _snapSettings      :: Snap
               } deriving (Show,Eq)
