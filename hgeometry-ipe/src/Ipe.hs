--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Reexports the functionality for reading and writing Ipe files.
--
--------------------------------------------------------------------------------
module Ipe( module Ipe.Types
                        , module Ipe.Writer
                        , module Ipe.Reader
                        , module Ipe.IpeOut
                        , module Ipe.FromIpe
                        , module Ipe.Attributes
                        , module Ipe.Value
                        , module Ipe.Color
                        ) where

import Ipe.Types
import Ipe.Writer
import Ipe.Reader
import Ipe.IpeOut
import Ipe.FromIpe
import Ipe.Attributes
import Ipe.Value
import Ipe.Color(IpeColor(..))
