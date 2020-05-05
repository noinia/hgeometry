module Miso.FFI.Extra where

import Control.Lens
import Language.Javascript.JSaddle

getBoundingClientRect  :: JSVal -> JSM JSVal
getBoundingClientRect e = e ^. js0 "getBoundingClientRect"
