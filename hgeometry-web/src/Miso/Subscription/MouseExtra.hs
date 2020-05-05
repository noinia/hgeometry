{-# LANGUAGE OverloadedStrings          #-}
module Miso.Subscription.MouseExtra where

import Control.Monad.IO.Class
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal
import Miso
import Miso.FFI.Extra
import Miso.String (MisoString)


-- | Gets the mouse position relative to the element named by the first argument
relativeMouseSub          :: MisoString -> ((Int,Int) -> action) -> Sub action
relativeMouseSub elemId f = \sink -> do
    windowAddEventListener "mousemove" $
      \mouseEvent -> do
        elem' <- getElementById elemId
        -- consoleLog elem'
        rect  <- getBoundingClientRect elem'
        -- consoleLog rect
        Just l <- fromJSVal =<< getProp "left" (Object rect)
        Just t <- fromJSVal =<< getProp "top"  (Object rect)

        Just cl <- fromJSVal =<< getProp "clientLeft" (Object elem')
        Just ct <- fromJSVal =<< getProp "clientTop"  (Object elem')

        Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
        Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)

        let x' = x-cl-l
            y' = y-ct-t
        -- see https://stackoverflow.com/questions/10298658/mouse-position-inside-autoscaled-svg
        liftIO (sink $ f (x',y'))
