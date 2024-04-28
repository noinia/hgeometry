{-# LANGUAGE TemplateHaskell #-}
module Options where

import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (makeRelativeToProject, embedFile)
import Miso.Bulma.JSAddle

--------------------------------------------------------------------------------

jsAddleOptions :: BulmaJSAddleOptions
jsAddleOptions = defaultOptions { indexHtml = myIndex
                                }
  where
    myIndex = fromStrict $
              $(makeRelativeToProject "skia/index.html" >>= embedFile)
