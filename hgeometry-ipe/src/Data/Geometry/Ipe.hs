--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Ipe
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Reexports the functionality for reading and writing Ipe files.
--
--------------------------------------------------------------------------------
module Data.Geometry.Ipe( module Data.Geometry.Ipe.Types
                        , module Data.Geometry.Ipe.Writer
                        , module Data.Geometry.Ipe.Reader
                        , module Data.Geometry.Ipe.IpeOut
                        , module Data.Geometry.Ipe.FromIpe
                        , module Data.Geometry.Ipe.Attributes
                        , module Data.Geometry.Ipe.Value
                        , module Data.Geometry.Ipe.Color
                        ) where

import Data.Geometry.Ipe.Types
import Data.Geometry.Ipe.Writer
import Data.Geometry.Ipe.Reader
import Data.Geometry.Ipe.IpeOut
import Data.Geometry.Ipe.FromIpe
import Data.Geometry.Ipe.Attributes
import Data.Geometry.Ipe.Value
import Data.Geometry.Ipe.Color(IpeColor(..))
