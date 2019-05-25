{-# LANGUAGE OverloadedStrings          #-}
module Miso.Subscription.MouseExtra where


import Control.Monad.IO.Class
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

import Miso
import Miso.String(MisoString)

relativeMouseSub          :: MisoString -> ((Int,Int) -> action) -> Sub action
relativeMouseSub elemId f = \sink -> do
    elem' <- getElementById elemId
    consoleLog elem'
    addEventListener elem' "mousemove" $
      \mouseEvent -> do
        Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
        Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
        liftIO (sink $ f (x,y))
