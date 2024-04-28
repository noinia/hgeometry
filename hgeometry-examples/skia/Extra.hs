--------------------------------------------------------------------------------
-- |
-- Module      :  Extra
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Some additional FFI helpers for miso
--
--------------------------------------------------------------------------------
module Extra where

import Control.Lens
import Language.Javascript.JSaddle

-- | call the "getBoundingClientRect" function of an object
getBoundingClientRect  :: JSVal -> JSM JSVal
getBoundingClientRect e = e ^. js0 "getBoundingClientRect"
